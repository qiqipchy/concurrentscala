package chapter03

/**
  * Created by wuyuanyuan on 18-10-12.
  */

import java.util.concurrent.atomic._

import chapter02.MakeThread

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object AtomicUid extends App {
    private val uid = new AtomicLong(0L)

    def getUniqueId = uid.getAndIncrement()

    val listBuffer = ListBuffer[Long]()
    for (i <- 1 to 100)
        MakeThread.thread {
            //            println(Thread.currentThread().getName)
            val a = getUniqueId

            synchronized {
                listBuffer.append(a)
            }
        }

    Thread.sleep(3000)
    println(listBuffer.toList.sorted)
    println(listBuffer.toList.length)
    //    val str = "12 14 13 6 0 8 5 7 2 1 3 4 9 15 10 11 17 16 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 45 44 43 46 48 47 49 52 54 55 53 51 50 56 57 58 59 62 61 60 63 66 65 64 70 72 67 69 71 68 74 75 73 76 79 78 77 80 81 82 83 84 85 86 87 88 89 90 91 92 94 93 95 96 97 98 99"

    //    println()
    //    val a = str.split(" +").map(num => num.toLong)
    //    println(a.length)
    //    println(str.split(" +").map(num => num.toLong).toList.sorted)
}

object AtomicUid1 extends App {
    private val uid = new AtomicLong(0L)

    def getUniqueId(): Long = uid.incrementAndGet()

    ExecutorUtil.execute {
        println(s"Uid asynchronously: ${getUniqueId()}")
    }
    println(s"Got a unique id: ${getUniqueId()}")

//    def compareAndSet(ov: Long, nv: Long): Boolean =
//        this.synchronized {
//            if (this.get == ov) false else {
//                this.set(nv)
//                true
//            }
//        }

//    @tailrec def getUniqueId(): Long = {
//        val oldUid = uid.get
//        val newUid = oldUid + 1
//        if (uid.compareAndSet(oldUid, newUid)) newUid
//        else getUniqueId()
//    }

}

