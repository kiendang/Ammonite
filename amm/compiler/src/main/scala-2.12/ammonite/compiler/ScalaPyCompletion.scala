package ammonite.compiler

trait ScalaPyCompletion extends ScalaPyCompletionBase {
  override def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] =
    tree.children.lastOption.flatMap(super.complete(_, evalClassloader, allCode, index))
}
