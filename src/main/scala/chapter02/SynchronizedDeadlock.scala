package chapter02

import chapter02.MakeThread._

object SynchronizedNesting extends App {

    import scala.collection._

    private val transfers = mutable.ArrayBuffer[String]()

    def logTransfer(name: String, n: Int) = transfers.synchronized {
        transfers += s"transfer to account '$name' = $n"
    }

    class Account(val name: String, var money: Int) {
        val id = getUniqueId
    }

    def add(account: Account, n: Int) = account.synchronized {
        account.money += n
        if (n > 10) logTransfer(account.name, n)
    }
}



object SynchronizedDeadlock extends App {

    import MakeThread._
    import SynchronizedNesting.Account

    // 容易产生死锁
    def send(a: Account, b: Account, n: Int) = a.synchronized {
        b.synchronized {
            a.money -= n
            b.money += n
        }
    }

    // 确定获取锁的顺序, 有效避免死锁
    def sendAll(sources: List[Account], target: Account) = {
        for (source <- sources) {
            if (source.id < target.id) {
                source.synchronized {
                    target.synchronized(send(source, a, source.money))
                }
            }
            else {
                target.synchronized {
                    source.synchronized(send(source, a, source.money))
                }
            }
        }
    }

    val a = new Account("Jack", 1000)
    val b = new Account("Jill", 2000)
    val c = new Account("Cindy", 3000)
    val d = new Account("Alex", 4000)
    val sources = List(b, c, d)
    val t1 = thread {
        for (i <- 0 until 100) sendAll(List(a), b)
    }
    val t2 = thread {
        for (i <- 0 until 100) sendAll(sources, a)
    }
    t1.join()
    t2.join()
    log(s"a = ${a.money}, b = ${b.money}")
}
