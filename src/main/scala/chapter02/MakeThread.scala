package chapter02

/**
  * Created by wuyuanyuan on 18-9-30.
  */
object MakeThread {
    var id = 0

    def thread(body: => Unit) = {
        val a = new Thread {
            override def run(): Unit = body
        }
        a.start()
        a

    }

    def getUniqueId = this.synchronized {
        id = id + 1
        id
    }

    def log(log: String) = println(log)

}