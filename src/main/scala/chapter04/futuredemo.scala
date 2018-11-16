package chapter04

import java.io.File

import chapter03.ExecutorUtil._

import scala.concurrent.Future
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Try}

object FuturesCreate extends App {
    Future {
        log("the future is here")
    }
    log("the future is coming")
    Thread.sleep(1000)
}

object FuturesCallbacks extends App {
    def getUrlSpec(): Future[List[String]] = Future {
        val url = "http://www.w3.org/Addressing/URL/url-spec.txt"
        val f = Source.fromURL(url)
        try f.getLines.toList finally f.close()
    }

    val urlSpec: Future[List[String]] = getUrlSpec()

    def find(lines: List[String], keyword: String): String =
        lines.zipWithIndex collect {
            case (line, n) if line.contains(keyword) => (n, line)
        } mkString ("\n")

    urlSpec foreach {
        case lines => log(find(lines, "telnet"))
    }

    urlSpec onComplete { lines =>
        println(Thread.currentThread().getName)
    }

    urlSpec onComplete { lines =>
        println(Thread.currentThread().getName)
    }

    log("callback registered, continuing with other work")
    Thread.sleep(2000)
}

object FuturesFailure extends App {
    val urlSpec: Future[String] = Future {
        val invalidUrl = "http://www.w3.org/Addressing/URL/url-spec.txt"
        Source.fromURL(invalidUrl).mkString
    }
    val a = urlSpec.failed
    Thread.sleep(1000)
    println(a.value)
    println(Failure[String](new Exception("ds")).isInstanceOf[Failure[Any]])
}

object FuturesNonFatal extends App {
    val f = Future {
        throw new InterruptedException
    }
    val g = Future {
        throw new IllegalArgumentException
    }
    f.failed foreach { case t => log(s"error - $t") }
    g.failed foreach { case t => log(s"error - $t") }
}

import org.apache.commons.io.FileUtils._
import scala.collection.convert.decorateAsScala._

object FuturesClumsyCallback extends App {
    def blacklistFile(name: String): Future[List[String]] = Future {
        val lines = Source.fromFile(name).getLines
        lines.filter(x => !x.startsWith("#") && !x.isEmpty).toList
    }

    def findFiles(patterns: List[String]): List[String] = {
        val root = new File(".")
        for {
            f <- iterateFiles(root, null, true).asScala.toList
            pat <- patterns
            abspat = root.getCanonicalPath + pat
            if f.getCanonicalPath.contains(abspat)
        } yield {
            f.getCanonicalPath
        }
    }

    blacklistFile(".gitignore") map {
        case lines =>
            val files = findFiles(lines)
            log(s"matches: ${files.mkString("\n")}")
    }
    Thread.sleep(1000)
}









