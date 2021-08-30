package ammonite.compiler.scalapy.utils

import ammonite.compiler.Completion

import scala.tools.nsc.interactive.Global
import scala.util.Random

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait WithPressy {
  val global: Global
  import global.{ApplyDynamic => _, _}

  def checkSymbolKind(kinds: Set[String])(sym: Symbol) =
    kinds.contains(sym.accurateKindString)

  val isValOrVar = checkSymbolKind(Set("getter", "setter")) _

  val isAttribute = checkSymbolKind(Set("module", "getter", "setter")) _

  def isCodeClassWrapperInstance(tree: global.Tree, cmd: String) =
    tree.tpe.baseClasses.exists(_.fullName == s"ammonite.$$sess.$cmd.Helper")
}
