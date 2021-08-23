package ammonite.compiler

import scala.collection.compat.immutable.LazyList
import scala.reflect.runtime.{universe => ru}
import scala.util.Random
import scala.tools.reflect.ToolBox

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait ScalaPyCompletionBase extends Completion {
  import global._
  import ScalaPyCompletionBase._

  def complete(
    tree: Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ): Option[(Int, Seq[(String, Option[String])])] = {
    lazy val toolbox = ru.runtimeMirror(evalClassloader).mkToolBox()

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
                  val matches = py.module("rlcompleter")
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
              if cmd.startsWith("cmd") && isValOrVar(wrapper.tpe.member(term)) =>
                Some(offset, Seq((prefixStr + root.symbol.accurateKindString, None)))
                val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
                val instance = cmdClass.getField("MODULE$").get(null)

                val rootValue = cmdClass
                  .getDeclaredMethod(v)
                  .invoke(instance)
                  .asInstanceOf[py.Dynamic]

                val matches = attrMatches(attrs.foldLeft(rootValue)(_.selectDynamic(_)), prefixStr)
                Some(offset, matches.map((_, None)))

            case q"${wrapper @ q"ammonite.$$sess.${TermName(cmd)}.instance"}.${term @ TermName(v)}"
              if (
                cmd.startsWith("cmd") &&
                  isCodeClassWrapperInstance(wrapper, cmd) &&
                  isValOrVar(wrapper.tpe.member(term))
              ) =>
                val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
                val instance = cmdClass.getField("MODULE$").get(null)
                val instanceMethod = cmdClass.getDeclaredMethod("instance")

                val rootValue = evalClassloader
                  .loadClass(s"ammonite.$$sess.${cmd}$$Helper")
                  .getDeclaredMethod(v)
                  .invoke(instanceMethod.invoke(instance))
                  .asInstanceOf[py.Dynamic]

                val matches = attrMatches(attrs.foldLeft(rootValue)(_.selectDynamic(_)), prefixStr)
                Some(offset, matches.map((_, None)))

            case SelectChain(
              q"ammonite", List((_, _, "$sess"), (_, _, cmd), rest @ _*)
            ) if cmd.startsWith("cmd") =>
              val (classBased, remains) = rest match {
                case (inst, _, "instance") :: remains if isCodeClassWrapperInstance(inst, cmd) =>
                  (true, remains)
                case remains => (false, remains)
              }

              val members = remains.map(_._3)

              val allAtributes =
                remains
                  .to(LazyList)
                  .map { case (_, qual, name) => qual.tpe.member(TermName(name)) }
                  .forall(isAttribute)

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

  private def checkSymbolKind(kinds: Set[String])(sym: Symbol) =
    kinds.contains(sym.accurateKindString)

  private val isValOrVar = checkSymbolKind(Set("getter", "setter")) _

  private val isAttribute = checkSymbolKind(Set("module", "getter", "setter")) _

  private def isCodeClassWrapperInstance(tree: global.Tree, cmd: String) =
    tree.tpe.baseClasses.exists(_.fullName == s"ammonite.$$sess.$cmd.Helper")

  object SelectDynamicChain {
    private object Aux {
      def unapply(t: Tree): Option[(Tree, List[String])] = t match {
        case q"${Aux(q, args)}.selectDynamic(${Literal(Constant(argN))})" =>
          Some((q, argN.toString :: args))
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

object ScalaPyCompletionBase {
  def namespace = py"globals()"

  def attrMatches(pyObject: py.Dynamic, attr: String): Seq[String] = {
    val variableName = randomNewVariableName()

    try {
      updateVariable(variableName, pyObject)
      attrMatches(variableName, attr)
    } finally {
      if (variableExists(variableName))
        deleteVariable(variableName)
    }
  }

  def attrMatches(expr: String, attr: String): Seq[String] = {
    val text = s"${expr}.${attr}"
    val pattern = s"^${expr}\\.(${attr}.*)$$".r

    py.module("rlcompleter")
      .Completer(namespace)
      .attr_matches(text)
      .as[Seq[String]]
      .map(pattern.findFirstMatchIn(_).map(_.subgroups))
      .collect { case Some(s :: Nil) => s }
  }

  def randomVariableName(length: Int = 5): String =
    "__scalapy" + Random.alphanumeric.take(length).mkString

  def randomNewVariableName(tries: Int = 10): String = {
    val generated = (1 to tries)
      .to(LazyList)
      .map(randomVariableName)
      .find(!variableExists(_))

    generated match {
      case Some(v) => v
      case None =>
        throw new Exception(s"Could not find a new random variable name in $tries tries")
    }
  }

  def variableExists(name: String): Boolean = py"$name in $namespace".as[Boolean]

  def deleteVariable(name: String): Unit = namespace.bracketDelete(name)

  def updateVariable(name: String, newValue: py.Any): Unit =
    namespace.bracketUpdate(name, newValue)

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
