package ammonite.compiler

import scala.tools.nsc.interactive.Global

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import scala.util.Try

trait ScalaPyJediCompletion extends Completion {
  import global._
  import ScalaPyJediCompletion._

  def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = jedi.toOption.flatMap { jedi =>
    None
  }
}

object ScalaPyJediCompletion {
  val jedi = Try(py.module("jedi"))
}
