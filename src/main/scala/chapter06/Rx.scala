package chapter06

/**
  * Created by wuyuanyuan on 18-11-19.
  */

import rx.lang.scala._

object RxUtil {
    def log[T](body: => T) = {
        println(Thread.currentThread().getName + " : " + body)
    }
}

import RxUtil._

object ObservablesItems extends App {
    val o = Observable.items("Pascal", "Java", "Scala")
    o.subscribe(name => log(s"learned the $name language"))
    o.subscribe(name => log(s"forgot the $name language"))
}

import scala.concurrent.duration._

object ObservablesTimer extends App {
    val o = Observable.timer(1.second)
    o.subscribe(_ => log("Timeout!"))
    o.subscribe(_ => log("Another timeout!"))
    Thread.sleep(2000)
}

object ObservablesExceptions extends App {
    val exc = new RuntimeException
    //    val o = Observable.items(1, 2) ++ Observable.error(exc)
    val o = Observable.items(1, 2) ++ Observable.error(exc) ++ Observable.items(3, 4)

    // val o = Observable.items(1, 2, exc) 这种是不会调用处理exception的回调函数的
    o.subscribe(
        x => log(s"number $x")
    )

}

object ObservablesLifetime extends App {
    val classics = List("Good, bad, ugly", "Titanic", "Die Hard")
    val movies = Observable.from(classics)
    movies.subscribe(new Observer[String] {
        override def onNext(value: String): Unit = println(s"value = ${value}")

        override def onError(error: Throwable): Unit = println(s"error = ${error}")

        override def onCompleted(): Unit = println("Completed!!!")
    })
}



