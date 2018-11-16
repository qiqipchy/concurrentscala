package chapter04

import java.util.{Timer, TimerTask}

import scala.concurrent.{Future, Promise}
import scala.io.{Codec, Source}
import scala.util.{Failure, Success}

/**
  * Implement a command-line program that asks the user to input a URL of
  * some website, and displays the HTML of that website. Between the time that
  * the user hits ENTER and the time that the HTML is retrieved, the program
  * should repetitively print a . to the standard output every 50 milliseconds,
  * with a two seconds timeout. Use only futures and promises, and avoid the
  * synchronization primitives from the previous chapters. You may reuse the
  * timeout method defined in this chapter.
  */

object FetchHTML {

    import scala.concurrent._
    import ExecutionContext.Implicits.global

    def main(args: Array[String]): Unit = {
        val string = "https://www.baidu.com/"
        val htmlFut = Future {
            Thread.sleep(1000)
            Source.fromURL(string)(Codec.ISO8859).getLines().toList.mkString("\n")
            //            Source.fromURL(string).getLines().toList.mkString("\n")
        }

        Future {
            timer.schedule(new TimerTask {
                override def run(): Unit = {
                    println(System.currentTimeMillis())
                    print(".")
                }
            }, 0, 100)
        }
        htmlFut.onComplete { v =>
            timer.cancel()
            println(System.currentTimeMillis())
            println(v.get)
            println()
        }
        Thread.sleep(2000)
    }


    private val timer = new Timer(true)

    def timeout[T](t: Long, body: => T): Future[T] = {
        val p = Promise[T]
        timer.schedule(new TimerTask {
            def run() = {
                p success body
            }
        }, t)
        p.future
    }

}

/**
  * Implement an abstraction called a single-assignment variable, represented
  * by the IVar class:
  * class IVar[T] {
  * def apply(): T = ???
  * def :=(x: T): Unit = ???
  * }
  * When created, the IVar class does not contain a value, and calling apply
  * results in an exception. After a value is assigned using the := method,
  * subsequent calls to := throw an exception, and the apply method returns
  * the previously assigned value. Use only futures and promises, and avoid
  * the synchronization primitives from the previous chapters.
  */

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class IVar[T] {

    val value = Promise[T]

    def apply(): T = if (!value.isCompleted) throw new Exception("has not a value")
    else Await.result(value.future, 10 seconds)

    def :=(x: T): Unit = if (!value.trySuccess(x)) throw new Exception("has already a value")
}

object TestIVar extends App {
    val ivar = new IVar[String]
    for {
        i <- 1 to 10
    } {
        chapter03.ExecutorUtil.execute {
            ivar := "jaj"
            println(ivar.apply())
        }
    }
    Thread.sleep(123)

}

/**
  * Extend the Future[T] type with the exists method, which takes a predicate
  * and returns a Future[Boolean] object:
  * def exists(p: T => Boolean): Future[Boolean]
  * The resulting future is completed with true if and only if the original future
  * is completed and the predicate returns true, and false otherwise. You
  * can use future combinators, but you are not allowed to create any Promise
  * objects in the implementation.
  */
object MuFuture {

    implicit class MyFuture[T](self: Future[T]) {
        def exists(p: T => Boolean): Future[Boolean] = self map (p)
    }

}

/**
  * Repeat the previous exercise, but use Promise objects instead of
  * future combinators.
  */
object MuFuture1 extends App {

    implicit class MyFuture[T](self: Future[T]) {
        def exists(p: T => Boolean): Future[Boolean] = {
            val res = Promise[Boolean]
            self.onComplete {
                case Success(result) => res success (p(result))
                case Failure(e) => res failure (e)
            }
            res.future
        }
    }

    val a = Future {
        "abc"
    }
    val b = Await.result(a.exists(x => x.length > 23), Duration.Inf)
    println(b)
}

/**
  * Repeat the previous exercise, but use the Scala Async framework.
  */
object MuFuture2 extends App {

    import scala.async.Async._

    implicit class MyFuture[T](self: Future[T]) {
        def exists(p: T => Boolean): Future[Boolean] = {
            val promise = Promise[Boolean]
            async {
                self onComplete (v => promise complete (v map p))
            }
            promise.future
        }
    }

    val a = Future {
        "abc"
    }
    val b = Await.result(a.exists(x => x.length > 2), Duration.Inf)
    println(b)

}

/**
  * Implement the spawn method, which takes a command-line string,
  * asynchronously executes it as a child process, and returns a future
  * with the exit code of the child process:
  * def spawn(command: String): Future[Int]
  * Make sure that your implementation does not cause thread starvation.
  */

/** *
  * Implement the IMap class, which represents a single-assignment map:
  * class IMap[K, V] {
  * def update(k: K, v: V): Unit
  * def apply(k: K): Future[V]
  * }
  * Pairs of keys and values can be added to the IMap object, but they can never
  * be removed or modified. A specific key can be assigned only once, and
  * subsequent calls to update with that key results in an exception. Calling
  * apply with a specific key returns a future, which is completed after that key
  * is inserted into the map. In addition to futures and promises, you may use
  * the scala.collection.concurrent.Map class.
  */

class IMap[K, V] {
    val map = new scala.collection.concurrent.TrieMap[K, V]()

    def update(k: K, v: V): Unit = {
        map.putIfAbsent(k, v) match {

            case None => throw new Exception("has value")
            case other => other
        }
    }

    def apply(k: K): Future[V] = {
        val promise = Promise[V]
        Future {
            promise complete {
                map.get(k) match {
                    case None => Failure(new Exception("has no value"))
                    case Some(v) => Success(v)
                }
            }
        }
        promise.future
    }
}

object IMapTest extends App {
    val map = new IMap[String, Int]
    for (i <- 1 to 10)
        chapter03.ExecutorUtil.execute {
            map.apply("ds").map(a => println(a + " " + i))
            //            Thread.sleep(12)
            //            if(i==1) map.update("ds", 1)
        }
    Thread.sleep(1000)
}

/**
  * Extend the Promise[T] type with the compose method, which takes a
  * function of the S => T type, and returns a Promise[S] object:
  * def compose[S](f: S => T): Promise[S]
  * Whenever the resulting promise is completed with some value x of the type
  * S (or failed), the original promise must be completed with the value f(x)
  * asynchronously (or failed), unless the original promise is already completed.
  */
object PromiseExtend extends App {

    implicit class PromiseOp[T](self: Promise[T]) {
        def compose[S](f: T => S): Promise[S] = {
            val promise = Promise[S]
            self.future.onComplete {
                v =>
                    println("ds")
                    promise complete (v map f)
            }
            promise
        }
    }

    val a = Promise[Int]
    a success (4)
    val b: Promise[String] = a compose (x => x + "ds")
    Thread.sleep(2)
    println(b.future.value.get.get)
}