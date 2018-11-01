package chapter03

import java.io.File
import java.util.concurrent.{ConcurrentHashMap, LinkedBlockingDeque, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicReference

import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection.convert.decorateAsScala._

sealed trait State

class Idle extends State

class Creating extends State

class Copying(var n: Int) extends State

class Deleting extends State

import ExecutorUtil._

class FileSystem(val root: String) {
    val messages = new LinkedBlockingQueue[String]()

    def logMessage(str: String) = messages.put(str)

    val logger = new Thread {
        override def run(): Unit = while (true) println(messages.take())
    }
    logger.setDaemon(true)
    logger.start()
    val rootDir = new File(root)
    val files: scala.collection.concurrent.Map[String, Entry] = new ConcurrentHashMap[String, Entry]().asScala

    for (f <- FileUtils.iterateFiles(rootDir, null, false).asScala)
        files.put(f.getName, new Entry(false))

    def deleteFile(filename: String): Unit = {
        files.get(filename) match {
            case None =>
                logMessage(s"Path '$filename' does not exist!")
            case Some(entry) if entry.isDir =>
                logMessage(s"Path '$filename' is a directory!")
            case Some(entry) => execute {
                if (prepareForDelete(entry)) {
                    if (FileUtils.deleteQuietly(new File(root + "/" + filename)))
                        files.remove(filename)
                    logMessage(s"Path '${root + "/" + filename}' is deleted!")
                }
            }
        }
    }

    @tailrec private def release(entry: Entry): Unit = {
        val s0 = entry.state.get
        s0 match {
            case c: Creating =>
                if (!entry.state.compareAndSet(s0, new Idle)) release(entry)
            case c: Copying =>
                val nstate = if (c.n == 1) new Idle else new Copying(c.n - 1)
                if (!entry.state.compareAndSet(s0, nstate)) release(entry)
        }
    }

    def copyFile(src: String, dest: String): Unit = {
        files.get(src) match {
            case Some(srcEntry) if !srcEntry.isDir => execute {
                if (acquire(srcEntry)) try {
                    val destEntry = new Entry(isDir = false)
                    destEntry.state.set(new Creating)
                    if (files.putIfAbsent(dest, destEntry) == None) try {
                        FileUtils.copyFile(new File(src), new File(dest))
                    } finally release(destEntry)
                } finally release(srcEntry)
            }
        }
    }

    @tailrec private def acquire(entry: Entry): Boolean = {
        val s0 = entry.state.get
        s0 match {
            case _: Creating | _: Deleting =>
                logMessage("File inaccessible, cannot copy."); false
            case i: Idle =>
                if (entry.state.compareAndSet(s0, new
                                Copying(1))) true
                else acquire(entry)
            case c: Copying =>
                if (entry.state.compareAndSet(s0, new
                                Copying(c.n + 1))) true
                else acquire(entry)
        }
    }

    def prepareForDelete(entry: Entry): Boolean = {
        entry.state.get match {
            // case s: Idle => true 这种代码是错误的, 因为随时可以有别的线程修改这个文件的状态
            case s: Idle =>
                if (entry.state.compareAndSet(s, new Deleting)) true
                else prepareForDelete(entry)
            // 在以下几种状态的时候直接返回false, 可以避免忙等待, 如果也使用递归等待的话, 就跟锁是一样的, 等于被阻塞了;
            // state在这种情况下就相当于是一个lock, 但是并没有阻塞线程或者是让线程处在忙等待
            // CAS比较的是引用相等性, 所以不要使用对象的equals方法
            case s: Creating =>
                println("File currently created, cannot delete.")
                false
            case s: Deleting =>
                false
            case s: Copying =>
                println("File currently copied, cannot delete.");
                false
        }
    }
}

class Entry(val isDir: Boolean) {

    val state = new AtomicReference[State](new Idle)

    // 返回的是之前的状态
    def releaseCopy(e: Entry): Copying = e.state.get match {
        case c: Copying =>
            val nstate = if (c.n == 1) new Idle else new Copying(c.n - 1)
            if (e.state.compareAndSet(c, nstate)) c
            else releaseCopy(e)
    }

    def acquireCopy(e: Entry, c: Copying): Any = e.state.get match {
        case i: Idle =>
            c.n = 1
            if (!e.state.compareAndSet(i, c)) acquireCopy(e, c)
        case oc: Copying =>
            c.n = oc.n + 1
            if (!e.state.compareAndSet(oc, c)) acquireCopy(e, c)
    }

}

object Test extends App {
    val fileSystem = new FileSystem(getClass.getResource("/").getPath + "/")
    fileSystem.deleteFile("test.txt")
    Thread.sleep(300)
}
