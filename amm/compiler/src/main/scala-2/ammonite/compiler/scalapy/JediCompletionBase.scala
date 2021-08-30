package ammonite.compiler.scalapy

import ammonite.compiler.Completion

import scala.collection.compat.immutable.LazyList
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox
import scala.util.Try

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait JediCompletionBase extends Completion { self =>
  import global.{ApplyDynamic => _, _}
  import JediCompletionBase._

  def complete(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = jedi.toOption.flatMap { jedi =>
    object PressyUtils extends utils.WithPressy { val global: self.global.type = self.global }
    object SelectChain extends utils.SelectChain { val global: self.global.type = self.global }
    lazy val toolbox = ru.runtimeMirror(evalClassloader).mkToolBox()

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
              val completions = getCompletions(code)(jedi)
              Some(offset, completions.map((_, None)))

            case q"${wrapper @ q"ammonite.$$sess.${TermName(cmd)}"}.${term @ TermName(v)}"
              if cmd.startsWith("cmd") && PressyUtils.isValOrVar(wrapper.tpe.member(term)) =>
                val rootValue = utils.getDeclaredValueDefaultWrapper(cmd, v, evalClassloader)
                  .asInstanceOf[py.Dynamic]

                val completions = getCompletions(rootValue, dynamics, prefixStr)(jedi)
                Some(offset, completions.map((_, None)))

            case q"${wrapper @ q"ammonite.$$sess.${TermName(cmd)}.instance"}.${term @ TermName(v)}"
              if (
                cmd.startsWith("cmd") &&
                  PressyUtils.isCodeClassWrapperInstance(wrapper, cmd) &&
                  PressyUtils.isValOrVar(wrapper.tpe.member(term))
              ) =>
                val rootValue = utils.getDeclaredValueClassWrapper(cmd, v, evalClassloader)
                  .asInstanceOf[py.Dynamic]

                val completions = getCompletions(rootValue, dynamics, prefixStr)(jedi)
                Some(offset, completions.map((_, None)))

            case SelectChain(
              q"ammonite", List((_, _, "$sess"), (_, _, cmd), rest @ _*)
            ) if cmd.startsWith("cmd") =>
              val (classBased, remains) = rest match {
                case (inst, _, "instance") :: remains
                  if PressyUtils.isCodeClassWrapperInstance(inst, cmd) => (true, remains)
                case remains => (false, remains)
              }

              val members = remains.map(_._3)

              val allAtributes =
                remains
                  .to(LazyList)
                  .map { case (_, qual, name) => qual.tpe.member(TermName(name)) }
                  .forall(PressyUtils.isAttribute)

              val names =
                if (classBased)
                  Seq("$sess", cmd, "instance") ++ members
                else Seq("$sess", cmd) ++ members

              if (allAtributes) {
                val rootValue =
                  toolbox
                    .eval(utils.SelectChain.RuntimeFactory.create("ammonite", names))
                    .asInstanceOf[py.Dynamic]

                val completions = getCompletions(rootValue, dynamics, prefixStr)(jedi)
                Some(offset, completions.map((_, None)))
              } else Some(offset, Nil)

            case _ => None
          }

      case _ => None
    }
  }

  object DynamicChain {
    private object Aux {
      def unapply(t: Tree): Option[(Tree, List[PythonDynamic])] = t match {
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

    def unapply(t: Tree): Option[(Tree, List[PythonDynamic])] = t match {
      case Aux(t, ds) => Some(t, ds.reverse)
      case _ => None
    }
  }
}

object JediCompletionBase {
  val jedi = Try(py.module("jedi"))

  def namespace = py"globals()"

  def getCompletions(code: String, namespace: py.Any)(jedi: py.Module): List[String] =
    py"list(map($getName, ${jedi.Interpreter(code, namespace).complete()}))".as[List[String]]

  def getCompletions(code: String)(jedi: py.Module): List[String] =
    getCompletions(code, py"[$namespace]")(jedi)

  def getCompletions(
    variable: py.Any,
    dynamics: List[PythonDynamic],
    prefixStr: String,
  )(jedi: py.Module): List[String] = {
    val variableName = PythonVariable.newRandomName()
    val ns = py"[{ $variableName: $variable }, $namespace]"
    val exprStr = convertToPython(variableName, dynamics)
    val code = if (exprStr.isEmpty) prefixStr else exprStr + "." + prefixStr

    getCompletions(code, ns)(jedi)
  }

  private val getName = py.module("operator").attrgetter("name")

  sealed trait PythonDynamic

  case class SelectDynamic(term: String) extends PythonDynamic
  case class ApplyDynamic(method: String) extends PythonDynamic
  case class ApplyDynamicNamed(method: String) extends PythonDynamic
  case object Call extends PythonDynamic
  case object BracketAccess extends PythonDynamic

  def convertToPython(dynamic: PythonDynamic): String = dynamic match {
    case SelectDynamic(term) => term
    case ApplyDynamic(method) => method + "()"
    case ApplyDynamicNamed(method) => method + "()"
    case Call => "()"
    case BracketAccess => "__getitem__()"
  }

  def convertToPython(dynamics: List[PythonDynamic]): String = dynamics match {
    case Nil => ""
    case head :: tails => tails.foldLeft(convertToPython(head)) {
      case (acc, Call) => acc + convertToPython(Call)
      case (acc, dynamic) => acc + "." + convertToPython(dynamic)
    }
  }

  def convertToPython(variable: String, dynamics: List[PythonDynamic]): String = dynamics match {
    case Nil => variable
    case Call :: _ => variable + convertToPython(dynamics)
    case _ :: _ if variable.isEmpty => convertToPython(dynamics)
    case _ :: _ => variable + "." + convertToPython(dynamics)
  }

  object PythonVariable extends utils.PythonVariable {
    def namespace = JediCompletionBase.namespace
  }
}
