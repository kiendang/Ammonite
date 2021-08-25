package ammonite.compiler.scalapy

import ammonite.compiler.Completion

import scala.tools.nsc.interactive.Global
import scala.util.Try

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait JediCompletion extends Completion {
  import global._
  import JediCompletion._

  def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = jedi.toOption.flatMap { jedi =>
    None
  }
}

object JediCompletion {
  val jedi = Try(py.module("jedi"))
}
