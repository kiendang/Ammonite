package ammonite.compiler.scalapy.utils

import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.interactive.Global

trait SelectChain {
  val global: Global
  import global._

  private object Aux {
    def unapply(t: Tree): Option[(Tree, List[(Tree, Tree, String)])] = t match {
      case q"${qualN @ Aux(q, acc)}.${TermName(nameN)}" =>
        Some(q, (t, qualN, nameN.toString) :: acc)
      case _ => Some(t, Nil)
    }
  }

  def unapply(t: Tree): Option[(Tree, List[(Tree, Tree, String)])] = t match {
    case Aux(q, selects) => Some(q, selects.reverse)
    case _ => None
  }
}

object SelectChain {
  trait Factory {
    val universe: scala.reflect.api.Universe
    import universe._

    def create(qualifier: String, names: Seq[String]): Tree =
      names.foldLeft(Ident(TermName(qualifier)): Tree) {
        case (qual, name) => q"${qual}.${TermName(name)}"
      }
  }

  object RuntimeFactory extends Factory { val universe: ru.type = ru }
}
