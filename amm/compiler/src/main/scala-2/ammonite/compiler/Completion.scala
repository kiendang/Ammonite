package ammonite.compiler

import scala.tools.nsc.interactive.Global

trait Completion {
  val global: Global

  def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])]
}
