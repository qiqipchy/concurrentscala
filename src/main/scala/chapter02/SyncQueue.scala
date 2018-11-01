package chapter02

class SyncQueue[T](val size: Int) {
    var values: Array[Option[T]] = new Array[Option[T]](size)
    values = values.map(_ => None)
    var putIndex = 0
    var getIndex = 0

    def isEmpty = size == 0

    def get(): T = values.synchronized {
        if (getIndex == putIndex && values(getIndex).isEmpty) {
            println("There is nothing to get.")
            values.wait()
        }
        println(s"get ${getIndex}")
        val a = values(getIndex).get
        values(getIndex) = None
        getIndex = (getIndex + 1) % size
        values.notify()
        a
    }

    def put(x: T): Unit = values.synchronized {
        //        println(values.toList)
        if (getIndex == putIndex && !values(putIndex).isEmpty) {
            println("There is nowhere to put.")
            values.wait()
        }
        values(putIndex) = Some(x)
        println(s"put ${putIndex}")
        putIndex = (putIndex + 1) % size
        values.notify()
    }
}

object Test extends App {
    val a = new SyncQueue[Int](15)

    val lock = new AnyRef
    var index = 0
    val aa = new Thread {
        override def run(): Unit = while (true) {
            a.put(index)
            index = index * 2
        }
    }

    val bb = new Thread {
        override def run(): Unit = while (true) {
            a.get()
        }

    }

    aa.start()
    bb.start()
}

case class Application(operation: String, use_local: Boolean)

case class ApplicationOp(application: Application)


