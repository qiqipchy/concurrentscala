package chapter05

import java.net.Socket

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.{IterableSplitter, ParSeq, SeqSplitter}
import scala.concurrent.{Future, Promise}
import scala.io.Source
import scala.util.Random

/**
  * Created by wuyuanyuan on 18-11-9.
  */
object xParallelUtil extends App {

    val a = System.currentTimeMillis()
    (1 to 1000000) filter (x => x.toString == x.toString.reverse)
    println(System.currentTimeMillis() - a)

    val b = System.currentTimeMillis()
    (1 to 1000000).par filter (x => x.toString == x.toString.reverse)
    println(System.currentTimeMillis() - b)

    @volatile var dummy: Any = _

    def timed[T](body: => T): Double = {
        val start = System.nanoTime
        dummy = body
        val end = System.nanoTime
        ((end - start) / 1000) / 1000.0
    }

}

import ParallelUtil._

object ParallelUtil {
    @volatile var dummy: Any = _

    def timed[T](body: => T): Double = {
        val start = System.nanoTime
        dummy = body
        val end = System.nanoTime
        ((end - start) / 1000) / 1000.0
    }

    def warmTimed[T](times: Int)(body: => T): Double = {
        val a = for (i <- 1 to times) yield timed(body)
        a.sum
    }

}

object ParBasic extends App {

    val numbers = Random.shuffle(Vector.tabulate(5000000)(i => i))
    //    val numbers = Random.shuffle((1 to 5000000))
    println(timed(numbers.max))
    println(timed(numbers.par.max))

    def a(a: Int, b: Int) = a + b

    a(1, 2)

}

import java.util.concurrent.atomic._
import scala.concurrent.ExecutionContext.Implicits.global

object ParUid extends App {
    private var uid = 0
    val seqtime = timed {
        for (i <- 0 until 10000000) uid = uid + 1
    }
    println(s"Sequential time $seqtime ms, $uid")
    val partime = timed {
        for (i <- (0 until 10000000).par) uid = uid + 1
    }
    println(s"Parallel time $partime ms, $uid")
}

import scala.concurrent.forkjoin.ForkJoinPool

object ParConfig extends App {
    val fjpool = new ForkJoinPool(2)
    val customTaskSupport = new parallel.ForkJoinTaskSupport(fjpool)
    val numbers = Random.shuffle(Vector.tabulate(5000000)(i => i))
    val partime = timed {
        val parnumbers = numbers.par
        parnumbers.tasksupport = customTaskSupport
        val n = parnumbers.max
        println(s"largest number $n")
    }
    println(s"Parallel time $partime ms")
}

object ParHtmlSearch extends App {
    def getHtmlSpec() = Future {
        val url = "http://www.w3.org/MarkUp/html-spec/html-spec.txt"
        val specSrc = Source.fromURL(url)
        try specSrc.getLines.toArray finally specSrc.close()
    }

    getHtmlSpec() foreach { case specDoc =>
        def search(d: GenSeq[String]): Double =
            timed {
                d.indexWhere(line => line.matches(".*TEXTAREA.*"))
            }

        val seqtime = search(specDoc)
        println(s"Sequential time $seqtime ms")
        val partime = search(specDoc.par)
        println(s"Parallel time $partime ms")
    }

    Thread.sleep(2000)
}

object ParNonParallelizableCollections extends App {
    val list = List.fill(1000000)("")
    val vector = Vector.fill(1000000)("")
    println(s"list conversion time: ${timed(list.par)} ms")
    println(s"vector conversion time: ${timed(vector.par)} ms")

    //list conversion time: 60.276 ms
    //vector conversion time: 0.026 ms
}

case class Query(query: String)

object ParNonParallelizableOperations extends App {

    def allMatches(a: GenSeq[Int]) = timed {
        //        val results = a.aggregate(0)((acc, num) => acc + num, (acc, num) => acc + num)
        val results = a.foldLeft(0)((acc, num) => acc + num)
        println(results)
    }

    val specDoc = (1 to 123000000).toVector
    val seqtime = allMatches(specDoc)
    println(s"Sequential time - $seqtime ms")
    val partime = allMatches(specDoc.par)
    println(s"Parallel time- $partime ms")

    //Sequential time - 29275.638 ms
    //Parallel time- 913.545 ms
}

object ParSideEffectsIncorrect extends App {
    def intersectionSize(a: GenSet[Int], b: GenSet[Int]): Int = {
        a.count(!b.contains(_))
    }

    val a = (0 until 1000).toSet
    val b = (0 until 1000 by 4).toSet
    val seqres = intersectionSize(a, b)
    val parres = intersectionSize(a.par, b.par)
    println(s"Sequential result - $seqres")
    println(s"Parallel result   - $parres")
}

object ParNonDeterministicOperation extends App {
    ParHtmlSearch.getHtmlSpec() foreach { case specDoc =>
        val patt = ".*TEXTAREA.*"
        val seqresult = specDoc.find(_.matches(patt))
        val parresult = specDoc.par.indexWhere(_.matches(patt))
        println(s"Sequential result - $seqresult")
        println(s"Parallel result  - $parresult")
    }
    Thread.sleep(3000)
}

object ParNonCommutativeOperator extends App {
    val doc = mutable.ArrayBuffer.tabulate(20)(i => s"Page $i, ")

    def test(doc: GenIterable[String]) {
        val seqtext = doc.seq.reduce(_ + _)
        val partext = doc.par.reduce(_ + _)
        println(s"Sequential result - $seqtext\n")
        println(s"Parallel result- $partext\n")
    }

    test(doc)
    test(doc.toSet)
}

object ParNonAssociativeOperator extends App {
    def test(doc: GenIterable[Int]) {
        val seqtext = doc.seq.reduceLeft(_ - _)
        val partext = doc.par.reduce(_ - _)
        println(s"Sequential result - $seqtext\n")
        println(s"Parallel result- $partext\n")
    }

    test(0 until 30)
}

object Test extends App {

    implicit class FutureOp[T](future: Future[T]) {
        def myMap[U](f: T => U) = {
            //Thread.sleep(2323)
            val p = Promise[U]

            future.onComplete { v =>
                p complete (v map f)
            }
            p.future
        }
    }

    val future1 = Future {
        1
    }
    val future2 = future1.map(_ + "34343")
    println("dsds" + future2.isCompleted)
}

object ConcurrentWrong extends App {

    import ParHtmlSearch.getHtmlSpec
    import chapter04.FuturesCallbacks.getUrlSpec

    def intersection(a: GenSet[String], b: GenSet[String]) = {
        val result = new mutable.HashSet[String]
        for (x <- a.par) if (b contains x) result.add(x)
        result
    }

    val ifut = for {
        htmlSpec <- getHtmlSpec()
        urlSpec <- getUrlSpec()
    } yield {
        val htmlWords = htmlSpec.mkString.split("\\s+").toSet
        val urlWords = urlSpec.mkString.split("\\s+").toSet
        intersection(htmlWords, urlWords)
    }
    ifut onComplete { case t => println(s"Result: ${t.get.size}") }
    Thread.sleep(5000)
}

object ConcurrentCorrect extends App {

    import ParHtmlSearch.getHtmlSpec
    import chapter04.FuturesCallbacks.getUrlSpec
    import java.util.concurrent.ConcurrentSkipListSet
    import scala.collection.convert.decorateAsScala._

    def intersection(a: GenSet[String], b: GenSet[String]) = {
        val skiplist = new ConcurrentSkipListSet[String]
        for (x <- a.par) if (b contains x) skiplist.add(x)
        val result: Set[String] = skiplist.asScala
        result
    }

    val ifut = for {
        htmlSpec <- getHtmlSpec()
        urlSpec <- getUrlSpec()
    } yield {
        val htmlWords = htmlSpec.mkString.split("\\s+").toSet
        val urlWords = urlSpec.mkString.split("\\s+").toSet
        intersection(htmlWords, urlWords)
    }
    ifut onComplete { case t => println(s"Result: ${t.get.size}") }
    Thread.sleep(12000)
}

object ConcurrentTrieMap extends App {
    val cache = new concurrent.TrieMap[Int, String]()
    for (i <- 0 until 100) cache(i) = i.toString
    for ((number, string) <- cache.par) {
        Thread.sleep(30)
        cache(-number) = s"-$string"
    }
    println(s"cache - ${cache.keys.toList.size}")
}

import scala.collection.parallel._

class ParString(val str: String) extends ParSeq[Char] {
    override def apply(i: Int): Char = str(i)

    def splitter: SeqSplitter[Char] = new ParStringSplitter(str, 0, str.length)

    override def seq: Seq[Char] = str.toSeq

    override def length: Int = str.length
}

class ParStringSplitter(val s: String, var i: Int, val limit: Int) extends SeqSplitter[Char] {
    override def dup: SeqSplitter[Char] = new ParStringSplitter(s, i, limit)

    override def split: Seq[SeqSplitter[Char]] = {
        val step = 2
        // 只是一种可行的方案
        psplit((limit - i) / 2, (limit - i) - (limit - i) / 2)

    }

    override def psplit(sizes: Int*): Seq[SeqSplitter[Char]] = {
        val res: Seq[SeqSplitter[Char]] = for (size <- sizes) yield {
            val lim = limit min (i + size)
            val res = new ParStringSplitter(s, i, lim)
            i = lim
            res
        }
        // SeqLike是:+ 和 +:接口
        if (i < limit) res :+ new ParStringSplitter(s, i, limit)
        else res
    }

    override def remaining: Int = limit - i

    override def hasNext: Boolean = i < limit

    override def next(): Char = {
        val ch = s(i)
        i = i + 1
        ch
    }
}

object CustomCharCount extends App {
    val txt = "A custom text " * 250000
    val partxt = new ParString(txt)
    val seqtime = warmTimed(50) {
        txt.foldLeft(0)((res, ch) => if (ch.isUpper) res + 1 else res)
    }
    val partime = warmTimed(50) {
        val a = partxt.aggregate(0)((n, c) => if (Character.isUpperCase(c)) n + 1 else n, _ + _)
    }
    println(seqtime)
    println(partime)
}

