package chapter04

import scala.util.{Success, Try}

object A extends App {
    val a = 1
    val b = a match {
        case 2 => 1
        case _ =>
    }
    println(b.getClass)
}