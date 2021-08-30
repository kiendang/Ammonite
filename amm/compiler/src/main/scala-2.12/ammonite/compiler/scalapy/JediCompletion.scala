package ammonite.compiler.scalapy

trait JediCompletion extends JediCompletionBase {
  override def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] =
    tree.children.lastOption.flatMap(super.complete(_, evalClassloader, allCode, index))
}
