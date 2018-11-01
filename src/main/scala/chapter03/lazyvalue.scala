package chapter03

/**
  * Created by wuyuanyuan on 18-10-26.
  */
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList, LinkedBlockingQueue}

import ExecutorUtil._
import chapter02.MakeThread

object LazyValsCreate extends App {
    lazy val obj = new AnyRef
    lazy val non = s"made by ${Thread.currentThread.getName}"
    execute {
        log(s"EC sees obj = $obj")
        log(s"EC sees non = $non")
    }
    log(s"Main sees obj = $obj")
    log(s"Main sees non = $non")
    Thread.sleep(500)
}

object LazyValsObject extends App {

    object Lazy {
        log("Running Lazy constructor.")
    }

    log("Main thread is about to reference Lazy.")
    Lazy
    log("Main thread completed.")
}

object LazyValsUnderTheHood extends App {
    // volatile是必须的, 不然一个线程更新了_bitmap, 但是另外一个线程不能读到
    @volatile private var _bitmap = false
    private var _obj: AnyRef = _

    def obj = if (_bitmap) _obj else {
        synchronized {
            if (!_bitmap) {
                _obj = new AnyRef
                _bitmap = true
            }
        }
        _obj
    }
}

object LazyValsDeadlock extends App {
    object A { lazy val x: Int = B.y }
    object B { lazy val y: Int = A.x }
    execute { B.y }
    Thread.sleep(2)
    A.x
}

object LazyValsAndBlocking extends App {
    lazy val x: Int = {
        val t = MakeThread.thread { println(s"Initializing .") }
        t.join()
        1
    }
    x
}

object LazyValsAndMonitors extends App {
    object LazyValsAndMonitorsInner {
        lazy val x = 1
        this.synchronized {
            val t = MakeThread.thread {
                x
            }
            t.join()
        }
    }
}

object CollectionsIterators extends App {
    val queue = new CopyOnWriteArrayList[String]
    for (i <- 1 to 5500) queue.add(i.toString)
    execute {
        Thread.sleep(10)
        val it = queue.iterator
        while (it.hasNext) log(it.next())
    }
    for (i <- 1 to 5500) queue.remove(queue.size()-1)
    Thread.sleep(1000)
}



