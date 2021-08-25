package ammonite.compiler.scalapy

import ammonite.compiler.Completion

import scala.tools.nsc.interactive.Global
import scala.util.Try

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait JediCompletion extends Completion {
  import global.{ApplyDynamic => _, _}
  import JediCompletion._

  def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = jedi.toOption.flatMap { jedi =>
    tree match {
      case t @ q"""${expr @ DynamicChain(root, dynamics)}
        .selectDynamic(${Literal(Constant(prefix))})
        """ if global.ask(() => t.tpe <:< global.typeOf[py.Any]) =>
          val offset = expr.pos.end + 1
          val prefixStr = {
            val s = prefix.toString
            if (s == "<error>") "" else s
          }

          root match {
            case _ if global.ask(() => root.tpe =:= global.typeOf[py.Dynamic.global.type]) =>
              val exprStr = convertToPython(dynamics)
              val code = if (exprStr.isEmpty) prefixStr else exprStr + "." + prefixStr
              val completions = getCompletions(jedi)(code)
              Some(offset, completions.map((_, None)))

            case _ => None
          }

      case _ => None
    }
  }

  object DynamicChain {
    private object Aux {
      def unapply(t: Tree): Option[(Tree, List[Dynamic])] = t match {
        case q"${Aux(q, ds)}.selectDynamic(${Literal(Constant(term))})" =>
          Some(q, SelectDynamic(term.toString) :: ds)
        case q"${Aux(q, ds)}.applyDynamic(${Literal(Constant(method))})()" =>
          Some(q, ApplyDynamic(method.toString) :: ds)
        case q"${Aux(q, ds)}.applyDynamic(${Literal(Constant(method))})($_)" =>
          Some(q, ApplyDynamic(method.toString) :: ds)
        case q"${Aux(q, ds)}.applyDynamicNamed(${Literal(Constant(method))})($_)" =>
          Some(q, ApplyDynamicNamed(method.toString) :: ds)
        case q"${Aux(q, ds)}.apply()" =>
          Some(q, Call :: ds)
        case q"${Aux(q, ds)}.apply($_)" =>
          Some(q, Call :: ds)
        case q"${Aux(q, ds)}.bracketAccess($_)" =>
          Some(q, BracketAccess :: ds)
        case _ => Some(t, Nil)
      }
    }

    def unapply(t: Tree): Option[(Tree, List[Dynamic])] = t match {
      case Aux(t, ds) => Some(t, ds.reverse)
      case _ => None
    }
  }
}

object JediCompletion {
  val jedi = Try(py.module("jedi"))

  def getCompletions(jedi: py.Module)(code: String): List[String] =
    py"list(map($getName, ${jedi.Interpreter(code, py"[$namespace]").complete()}))".as[List[String]]

  private val getName = py.module("operator").attrgetter("name")

  def namespace = py"globals()"

  sealed trait Dynamic
  case class SelectDynamic(term: String) extends Dynamic
  case class ApplyDynamic(method: String) extends Dynamic
  case class ApplyDynamicNamed(method: String) extends Dynamic
  case object Call extends Dynamic
  case object BracketAccess extends Dynamic

  def convertToPython(dynamic: Dynamic): String = dynamic match {
    case SelectDynamic(term) => term
    case ApplyDynamic(method) => method + "()"
    case ApplyDynamicNamed(method) => method + "()"
    case Call => "()"
    case BracketAccess => "__getitem__()"
  }

  def convertToPython(dynamics: List[Dynamic]): String = dynamics match {
    case Nil => ""
    case head :: tails => tails.foldLeft(convertToPython(head)) {
      case (acc, Call) => acc + convertToPython(Call)
      case (acc, dynamic) => acc + "." + convertToPython(dynamic)
    }
  }

  def convertToPython(variable: String, dynamics: List[Dynamic]): String = dynamics match {
    case Nil => variable
    case Call :: _ => variable + convertToPython(dynamics)
    case _ :: _ if variable.isEmpty => convertToPython(dynamics)
    case _ :: _ => variable + "." + convertToPython(dynamics)
  }
}
