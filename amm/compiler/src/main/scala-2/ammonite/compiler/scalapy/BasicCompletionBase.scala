package ammonite.compiler.scalapy

import ammonite.compiler.Completion

import scala.collection.compat.immutable.LazyList
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait BasicCompletionBase extends Completion { self =>
  import global._
  import BasicCompletionBase._

  def complete(
    tree: Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = {
    lazy val toolbox = ru.runtimeMirror(evalClassloader).mkToolBox()

    object PressyUtils extends utils.PressyUtils { val global: self.global.type = self.global }

    tree match {
      case t @ q"""${expr @ SelectDynamicChain(root, attrs)}
        .selectDynamic(${Literal(Constant(prefix))})
        """ if global.ask(() => t.tpe <:< global.typeOf[py.Any]) =>
          val offset = expr.pos.end + 1
          val prefixStr = {
            val s = prefix.toString
            if (s == "<error>") "" else s
          }

          root match {
            case _ if global.ask(() => root.tpe =:= global.typeOf[py.Dynamic.global.type]) =>

              attrs match {
                case Nil if !prefixStr.isEmpty =>
                  val matches = rlcompleter
                    .Completer(namespace)
                    .global_matches(prefixStr)
                    .as[Seq[String]]
                    .filter(_.startsWith(prefixStr))

                  Some(offset, matches.map((_, None)))

                case _ :: _ =>
                  val exprStr = attrs.mkString(".")
                  val matches = attrMatches(exprStr, prefixStr)

                  Some(offset, matches.map((_, None)))

                case _ => None
              }

            case q"${wrapper @ q"ammonite.$$sess.${TermName(cmd)}"}.${term @ TermName(v)}"
              if cmd.startsWith("cmd") && PressyUtils.isValOrVar(wrapper.tpe.member(term)) =>
                val rootValue = utils.getDeclaredValueDefaultWrapper(cmd, v, evalClassloader)
                  .asInstanceOf[py.Dynamic]

                val matches = attrMatches(attrs.foldLeft(rootValue)(_.selectDynamic(_)), prefixStr)
                Some(offset, matches.map((_, None)))

            case q"${wrapper @ q"ammonite.$$sess.${TermName(cmd)}.instance"}.${term @ TermName(v)}"
              if (
                cmd.startsWith("cmd") &&
                  PressyUtils.isCodeClassWrapperInstance(wrapper, cmd) &&
                  PressyUtils.isValOrVar(wrapper.tpe.member(term))
              ) =>
                val rootValue = utils.getDeclaredValueClassWrapper(cmd, v, evalClassloader)
                  .asInstanceOf[py.Dynamic]

                val matches = attrMatches(attrs.foldLeft(rootValue)(_.selectDynamic(_)), prefixStr)
                Some(offset, matches.map((_, None)))

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

              if (allAtributes) {
                val rootValue =
                  toolbox
                    .eval(treeReconstruction.selects(members, cmd, classBased))
                    .asInstanceOf[py.Dynamic]
                val matches =
                  attrMatches(attrs.foldLeft(rootValue)(_.selectDynamic(_)), prefixStr)
                Some(offset, matches.map((_, None)))
              } else Some(offset, Nil)

            case _ => Some(offset, Nil)
          }

      case _ => None
    }
  }

  lazy val treeReconstruction = new TreeReconstruction { val universe: ru.type = ru }

  object SelectDynamicChain {
    private object Aux {
      def unapply(t: Tree): Option[(Tree, List[String])] = t match {
        case q"${Aux(q, args)}.selectDynamic(${Literal(Constant(argN))})" =>
          Some(q, argN.toString :: args)
        case _ => Some(t, Nil)
      }
    }

    def unapply(t: Tree): Option[(Tree, List[String])] = t match {
      case Aux(q, args) => Some(q, args.reverse)
      case _ => None
    }
  }

  object SelectChain {
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
}

object BasicCompletionBase {
  def namespace = py"globals()"

  val rlcompleter = py.module("rlcompleter")

  def attrMatches(pyObject: py.Dynamic, attr: String): Seq[String] = {
    val variableName = PythonVariable.newRandomName()

    try {
      PythonVariable.update(variableName, pyObject)
      attrMatches(variableName, attr)
    } finally {
      if (PythonVariable.exists(variableName))
        PythonVariable.delete(variableName)
    }
  }

  def attrMatches(expr: String, attr: String): Seq[String] = {
    val text = s"${expr}.${attr}"
    val pattern = s"^${expr}\\.(${attr}.*)$$".r

    rlcompleter
      .Completer(namespace)
      .attr_matches(text)
      .as[Seq[String]]
      .map(pattern.findFirstMatchIn(_).map(_.subgroups))
      .collect { case Some(s :: Nil) => s }
  }

  object PythonVariable extends utils.PythonVariable {
    def namespace = BasicCompletionBase.namespace
  }

  trait TreeReconstruction {
    val universe: scala.reflect.api.Universe
    import universe._

    def selects(members: Seq[String], cmd: String, classBased: Boolean = false): Tree = {
      val base = q"ammonite.$$sess.${TermName(cmd)}"
      val wrapper = if (classBased) q"${base}.instance" else base

      members.foldLeft(wrapper) { case (qual, name) => q"${qual}.${TermName(name)}" }
    }
  }
}
