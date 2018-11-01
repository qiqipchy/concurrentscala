package chapter02

// Chapter2的Exercises 4
class SyncVar[T] {
    var value: Option[T] = None

    var isEmpty = true

    def get(): T = {
        isEmpty = true
        value.get
    }

    def put(x: T): Unit = {
        value = Some(x)
        isEmpty = false
    }
}

object TestSyncVar extends App {
    val a = new SyncVar[Int]

    val lock = new AnyRef
    var index = 0
    val aa = new Thread {
        override def run(): Unit = lock.synchronized {
            while (index <= 15) {
                if (!a.isEmpty) {
                    lock.wait()
                }
                a.put(index)
                index = index + 1
                lock.notify()
            }
        }
    }

    val bb = new Thread {
        override def run(): Unit = lock.synchronized {
            while (index <= 15) {
                if (a.isEmpty) {
                    lock.wait()
                }
                println(a.get)
                lock.notify()
            }
        }
    }

    aa.start()
    bb.start()
}