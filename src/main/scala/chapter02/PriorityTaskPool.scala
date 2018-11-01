package chapter02

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by wuyuanyuan on 18-9-30.
  */
class PriorityTaskPool(val pool_size: Int, val important: Int) {
    private val tasks = mutable.Queue[TaskWithPriority]()
    private val categories = List(ListBuffer[Int](), ListBuffer[Int](), ListBuffer[Int](), ListBuffer[Int]())
    private val workers: Seq[Thread] =
        for (i <- 1 to pool_size) yield {
            val worker = new Thread {

                private def poll = tasks.synchronized {
                    if (tasks.isEmpty) try {
                        tasks.wait()
                    } catch {
                        case e: InterruptedException => println(e)
                    }
                    tasks.dequeue()
                }

                override def run(): Unit = while (true) {
                    if (!isInterrupted) {
                        val task = poll
                        val index = Thread.currentThread.getName.toList.last.toInt - '1'
                        categories(index).append(task.priority)
                        task.task()
                    }
                    else {
                        if (tasks.head.priority < important)
                            return
                    }
                }
            }
            worker.setDaemon(true)
            worker.setName("worker" + i)
            worker.start()
            worker
        }

    def asynchronous(body: => Unit) = tasks.synchronized {
        tasks.enqueue(TaskWithPriority(0, () => Unit))
        tasks.notify()
    }

    // 优先级高的执行
    def asynchronous(priority: Int)(task: => Unit) = tasks.synchronized {
        // 这里其实是指针的位置，并不是copy了数据，所以会随着tasks的改变而改变、
        val (left, right) = tasks.span(location => location.priority >= priority)
        val realRight = right.toList
        tasks.dequeueAll(_ => true)
        for (leftItem <- left)
            tasks.enqueue(leftItem)
        tasks.enqueue(TaskWithPriority(priority, () => task))
        for (rightItem <- realRight)
            tasks.enqueue(rightItem)
        tasks.notify()
    }

    // 优先级大于important的执行完毕, 抛弃剩下的任务
    def shutdown(): Unit = {
        for (worker <- workers)
            worker.interrupt()

    }

}

case class TaskWithPriority(val priority: Int, val task: () => Unit)

object PriorityTaskPool extends App {
    def apply(pool_size: Int): PriorityTaskPool = new PriorityTaskPool(pool_size, 5)

    val testInstance = PriorityTaskPool(4)
    Thread.sleep(400)

    for (i <- 1 to 100) {
        // println("d") 加一句输出就可以看到
        // worker1中poll得到的任务为ListBuffer(1, 9, 7, 5, 4, 2, 1, 5, 7, 9, 1, 3, 6, 7, 2, 0, 5, 6, 8, 0, 2, 4, 6, 8, 0, 2, 4, 5, 8, 9, 1, 3, 5)
        // 表示执行权转移给了worker线程，边put边get，说明了打印是一个耗时比较长的动作
        testInstance.asynchronous(i % 10) {
            println(s"Hello: priority${i % 10} +    ${i}    ${Thread.currentThread().getName}")
        }
    }
    for (worker <- testInstance.workers) worker.interrupt()
    Thread.sleep(500)
    println(testInstance.categories)
}



