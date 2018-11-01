package chapter03

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.{ExecutionContext, forkjoin}

/**
  * Created by wuyuanyuan on 18-9-30.
  */
object ExecutorsCreate extends App {
    val executor = new forkjoin.ForkJoinPool
    executor.execute {
        new Runnable {
            override def run(): Unit = println(Thread.currentThread.getName)
        }
    }
    executor.awaitTermination(400, TimeUnit.MILLISECONDS)

}

object ExecutionContextGlobal extends App {

    val exct = scala.concurrent.ExecutionContext.global
    exct.execute(new Runnable {
        override def run(): Unit = println(Thread.currentThread.getName)
    })
    Thread.sleep(500)
}

object ExecutionContextCreate extends App {
    val fromExecutor = ExecutionContext.fromExecutor(new forkjoin.ForkJoinPool)
    fromExecutor.execute(new Runnable {
        override def run(): Unit = println(Thread.currentThread.getName)
    })
    val fromExecutorService = ExecutionContext.fromExecutorService(new forkjoin.ForkJoinPool(4))
    fromExecutorService.execute(new Runnable {
        override def run(): Unit = println(Thread.currentThread.getName)
    })
    Thread.sleep(400)

}
import ExecutorUtil._
object ExecutorUtil {
    def execute(body: => Unit) = ExecutionContext.global.execute(new Runnable {
        override def run(): Unit = body
    })

    def log(body: AnyRef) = println(s"${Thread.currentThread} ${body}")
}

object ExecutionContextSleep extends App {
    val cur = System.currentTimeMillis
    for (i <- 1 to 32) execute {
        Thread.sleep(2000)
        println(i)
    }
    Thread.sleep(10000)
    println((System.currentTimeMillis() - cur) / 1000)
}

object AtomicLock extends App {
    private val lock = new AtomicBoolean(false)

    def mySynchronized(body: => Unit): Unit = {
        while (!lock.compareAndSet(false, true)) {}
        try body finally lock.set(false)
    }

    var count = 0
    for (i <- 0 until 1000) execute {
        mySynchronized {
            count += 1
        }
    }
    Thread.sleep(1000)
    println(s"Count is: $count")

}


