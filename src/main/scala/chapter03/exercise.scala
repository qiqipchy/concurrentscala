package chapter03

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Implement a custom ExecutionContext class called PiggybackContext,
  * which executes Runnable objects on the same thread that calls execute.
  * Ensure that a Runnable object executing on the PiggybackContext can
  * also call execute and that exceptions are properly reported.
  */

object PiggybackContext extends App {
    val tasks = new ArrayBlockingQueue[Runnable](90)

    val thread = new Thread {
        this.setDaemon(true)

        // 哪个线程被打断，哪个线程需要捕获异常
        // 这种会导致tasks不能再接收新的任务, 如果queue设置比较小的容量的话，会阻塞任务提交的线程
        override def run(): Unit = while (!isInterrupted) {
            try {
                val task = tasks.take
                task.run
            } catch {
                case e => e.printStackTrace()
            }
        }

    }

    thread.start()

    def execute(task: Runnable) = Try(tasks.put(task))

    for (i <- 1 to 100) {
        if (i == 10) thread.interrupt()
        println(i)
        // execute是执行在main线程的，所以当ArrayBlockingQueue的容量比较小的时候，interrupt了，就不会
        // 从tasks中取出数据了,execute也放不进去，所以主线程就一直被阻塞。
        execute(new Runnable {
            override def run(): Unit = println(i + Thread.currentThread().getName)
        })
    }

    //    Thread.sleep(0)
    thread.interrupt()

}

/**
  * Implement a TreiberStack class, which implements a concurrent stack
  * abstraction:
  * class TreiberStack[T] {
  * def push(x: T): Unit = ???
  * def pop(): T = ???
  * }
  * Use an atomic reference variable that points to a linked list of nodes that
  * were previously pushed to the stack. Make sure that your implementation
  * is lock-free and not susceptible to the ABA problem.
  */

class TreiberStack[T] {
    private val stack = new AtomicReference[List[T]](Nil)

    def push(x: T): Unit = {
        var oldV = List[T]()
        var newV = List[T]()
        do {
            oldV = stack.get()
            newV = x :: oldV
        } while (!stack.compareAndSet(oldV, newV))
    }

    def pop(): T = {
        var oldV = List[T]()
        var newV = List[T]()
        do {
            oldV = stack.get()
            if (oldV.isEmpty)
                throw new Exception("the stack is empty")
            newV = oldV.tail
        } while (!stack.compareAndSet(oldV, newV))
        oldV.head
    }

}

object TestStack extends App {
    val stack = new TreiberStack[String]()
    val list = new ListBuffer[Int]()
    println(Runtime.getRuntime.availableProcessors())
    for (i <- 1 to 100800) {
        if (i % 4 != 0)
            ExecutorUtil.execute(stack.push(i + ""))
        else {
            ExecutorUtil.execute {
                val poolNum = Thread.currentThread().getName.split("-")(3).toInt
                println(Thread.currentThread().getName + "/" + stack.pop() + " ")
                list.synchronized(list.append(poolNum))
            }
        }
        //        println(stack.pop().isInstanceOf[Unit])
    }
    Thread.sleep(3000)
    println()
    println(list.size)
    println(list.toSet)
}

/** *
  * Implement a ConcurrentSortedList class, which implements a concurrent
  * sorted list abstraction:
  * class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
  * def add(x: T): Unit = ???
  * def iterator: Iterator[T] = ???
  * }
  * Under the hood, the ConcurrentSortedList class should use a linked list of
  * atomic references. Ensure that your implementation is lock-free and avoids
  * ABA problems.
  * The Iterator object returned by the iterator method must correctly
  * traverse the elements of the list in the ascending order under the assumption
  * that there are no concurrent invocations of the add method.
  */
class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
    val list = new AtomicReference[List[T]](List[T]())

    def add(x: T): Unit = {
        var oldV = List[T]()
        var newV = List[T]()
        do {
            oldV = list.get()

            val (left, right) = oldV.span(ord.compare(_, x) <= 0)
            newV = left ::: x :: right

        } while (!list.compareAndSet(oldV, newV))
    }

    // 没有add调用
    def iterator: Iterator[T] = {
        var oldV = List[T]()
        var newV = List[T]()
        do {
            oldV = list.get()
            newV = List(oldV: _*)
        } while (!list.compareAndSet(oldV, newV))
        newV.iterator
    }

}

object TestConcurrentSortedList extends App {
    val stack = new ConcurrentSortedList[Int]()
    val list = new ListBuffer[Int]()
    println(Runtime.getRuntime.availableProcessors())
    for (i <- 1 to 100) {
        if (i % 4 != 0)
            ExecutorUtil.execute(stack.add(i))
        else {
            ExecutorUtil.execute {
                val iter = stack.iterator
                var list = new ListBuffer[Int]()
                //                println(this)
                Console.synchronized {
                    while (iter.hasNext) {
                        val a = iter.next()
                        //　会并发进行访问，所以print胡来的内容是多个线程的混在一起的结果
                        // 但是某一个单独的线程里面list是正确的，因为list没有共享
                        // 但是print是共享的！！！使用iter.synchronized是不行的，只能this.synchronized
                        //                        print(Thread.currentThread().getName + "--" + a + ",")
                        // 输出到控制台是正确的Console.synchronized
                        print(a + ",")
                        list.append(a)
                    }
                }
                println("----")

                val listSorted = list.sorted
                println(list)
                assert(list == listSorted)
            }
        }
        //        println(stack.pop().isInstanceOf[Unit])
    }
    Thread.sleep(1000)
    println(stack.list)

    val a = List(1, 2, 3)
    val b = a
    println(a eq b)
}

/** *
  * Implement a LazyCell class with the following interface:
  * class LazyCell[T](initialization: =>T) {
  * def apply(): T = ???
  * }
  * Creating a LazyCell object and calling the apply method must have the
  * same semantics as declaring a lazy value and reading it, respectively.
  * You are not allowed to use lazy values in your implementation.
  */
class LazyCell[T](initialization: => T) {
    private var _bitmap = false
    private var _obj: T = _

    def apply(): T = {
        if (_bitmap) _obj
        else {
            this.synchronized {
                if (!_bitmap) {
                    _obj = initialization
                    _bitmap = true
                }
                _obj
            }
        }
    }
}

object TestLazyCell extends App {
    val list = new ListBuffer[ConcurrentSortedList[Int]]
    val concurrentSortedList = new ConcurrentSortedList[Int]()
    val singleton1 = new LazyCell[ConcurrentSortedList[Int]](concurrentSortedList)
    val singleton = new LazyCell[ConcurrentSortedList[Int]](concurrentSortedList)
    for (i <- 1 to 1000) {
        ExecutorUtil.execute {
            val a = singleton1()
            val b = singleton()
            list.append(a)
            list.append(b)
        }
    }

    Thread.sleep(500)
    println(list.forall(_ eq concurrentSortedList))
}

/** *
  * Implement a PureLazyCell class with the same interface and semantics
  * as the LazyCell class from the previous exercise. The PureLazyCell class
  * assumes that the initialization parameter does not cause side effects, so it
  * can be evaluated more than once.
  * The apply method must be lock-free and should call the initialization
  * as little as possible.
  */

class PureLazyCell[T](initialization: => T) {
    val valueOpt = new AtomicReference[Option[T]](None)

    def apply(): T = valueOpt.get() match {
        case None =>
            Console.synchronized(println("----" + initialization))
            val a = valueOpt.compareAndSet(None, Some(initialization))
            // 不能这样调用initialization，如果在compareAndSet以及调用initialization之间别的线程插进来运行改变了
            // initialization的状态, 则PureLazyCell会有两个T存在，可以在这里Thread.sleep(10)试一下
            Console.synchronized(println(a + "///" + initialization))
            Thread.sleep(5)
            Console.synchronized(println("///" + initialization))
            initialization
        //            apply()
        case Some(x) => x
    }
}

class Animal {
    var x = 1

    override def toString: String = x + ""
}

object TestPureLazyCell extends App {
    val list = new ListBuffer[Animal]
    val animal = new Animal()
    val singleton1 = new PureLazyCell[Animal](animal)
    for (i <- 1 to 100) {
        ExecutorUtil.execute {
            val a = singleton1()
            list.synchronized(list.append(a))
        }
    }
    Thread.sleep(5)
    animal.x = 2
    Thread.sleep(500)
    println(list.size)
    println(list.forall(_.x == 2))
}







