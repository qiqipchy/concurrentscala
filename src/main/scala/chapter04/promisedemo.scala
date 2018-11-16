package chapter04

import java.io.File
import java.util.{Timer, TimerTask}

import chapter03.ExecutorUtil._
import org.apache.commons.io.monitor.{FileAlterationListenerAdaptor, FileAlterationMonitor, FileAlterationObserver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, CancellationException, Future, Promise}
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by wuyuanyuan on 18-11-5.
  */
object PromisesCreate extends App {
    val p = Promise[String]
    val q = Promise[String]
    p.future foreach { case x => log(s"p succeeded with '$x'") }
    Thread.sleep(1000)
    p complete Success("assigned")
    q failure new Exception("not kept")
    q.future.failed foreach { case t => log(s"q failed with $t") }
    val r = Promise[String]
    r complete Failure(new Exception("ds"))
    r.future.failed foreach { case x => log(s"r failed with '$x'") }
    Thread.sleep(2000)
}

object PromisesCustomAsync extends App {
    def myFuture[T](b: => T): Future[T] = {
        val p = Promise[T]
        global.execute(new Runnable {
            override def run(): Unit = p.complete {
                try Success(b) catch {
                    case NonFatal(e) => Failure(e)
                }
            }
        })
        p.future
    }

    def fileCreated(directory: String): Future[String] = {
        val p = Promise[String]
        val fileMonitor = new FileAlterationMonitor(1000)
        val observer = new FileAlterationObserver(directory)
        val listener = new FileAlterationListenerAdaptor {
            override def onFileCreate(file: File): Unit =
                try p.trySuccess(file.getName) finally fileMonitor.stop()
        }
        observer.addListener(listener)
        fileMonitor.addObserver(observer)
        fileMonitor.start()
        p.future
    }

    private val timer = new Timer(true)

    def timeout(t: Long): Future[Int] = {
        val p = Promise[Int]
        timer.schedule(new TimerTask {
            def run() = {
                val sum = (1 to 100).sum
                p success sum
                timer.cancel()
            }
        }, t)
        p.future
    }

    import MyFutureTest._

    timeout(1000).or(Future {
        23
    })
}

object MyFutureTest extends App {
    println(PromisesCustomAsync.myFuture(1))
    Future {
        1
    }.or(Future {
        2
    })

    implicit class FutureOps[T](val self: Future[T]) {
        def or(that: Future[T]): Future[T] = {
            val p = Promise[T]
            self onComplete { case x => p complete x }
            that onComplete { case y => p complete y }
            p.future
        }
    }

}

object PromisesCancellation extends App {
    type Cancellable[T] = (Promise[Unit], Future[T])

    def cancellable[T](b: Future[Unit] => T): Cancellable[T] = {
        val cancel = Promise[Unit]
        val f = Future {
            val r = b(cancel.future)
            if (!cancel.tryFailure(new Exception))
                throw new CancellationException
            r
        }
        (cancel, f)
    }

    cancellable { cancel =>
        Thread.sleep(12)
        (1 to 10000).sum
        println(Thread.currentThread().getName)
        //        cancel onComplete()
    }._2

    Thread.sleep(1000)
}

import scala.concurrent.duration._

object BlockingAwait extends App {
    val urlSpecSizeFuture = Future {
        val specUrl = "http://www.w3.org/Addressing/URL/url-spec.txt"
        Source.fromURL(specUrl).size
    }
    // 返回的是future
    val a: Future[Int] = Await.ready(urlSpecSizeFuture, 5.seconds)
    // 返回的是T
    val urlSpecSize = Await.result(urlSpecSizeFuture, 5.seconds)
    log(s"url spec contains $urlSpecSize characters")
}

object BlockingReady extends App {

    import scala.concurrent._
    import ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val startTime = System.nanoTime
    val futures = for (_ <- 0 until 16) yield Future {
        blocking {
            Thread.sleep(1000)
        }
    }
    for (f <- futures) Await.ready(f, Duration.Inf)
    val endTime = System.nanoTime
    log(s"Total time = ${(endTime - startTime) / 1000000} ms")
    log(s"Total CPUs = ${Runtime.getRuntime.availableProcessors}")

}

object DelayUseAsyncLib extends App {

    import scala.async.Async._
    import scala.concurrent._
    import ExecutionContext.Implicits.global

    def delay() = async {
        blocking {
            Thread.sleep(200)
            println("200")
            4
        }
    }
    def myWait() = async {
        println("I'm here")
        await(delay())
        println("I'm here after 200ms")
    }


    val a = myWait()
    Thread.sleep(1222)
    println(a.value.get.get)
}


