package ammonite.compiler

import scala.collection.compat.immutable.LazyList
import scala.util.Random
import scala.tools.nsc.interactive.Global

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait Completion {
  val global: Global

  abstract class Run(
    tree: global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ) {
    def prefixed: Option[(Int, Seq[(String, Option[String])])]
  }
}

trait ScalaPyCompletion extends Completion {
  import global._
  import ScalaPyCompletion._

  class Run(
    tree: Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ) {
    def prefixed: Option[(Int, Seq[(String, Option[String])])] = tree match {
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
              if cmd.startsWith("cmd") && isValOrVar(wrapper.tpe.member(term)) =>
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

            case _ => None
          }

      case _ => None
    }
  }

  private lazy val valOrVarSymbolKinds = Set("getter", "setter")

  private def isValOrVar(sym: Symbol) = valOrVarSymbolKinds.contains(sym.accurateKindString)

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
}

object ScalaPyCompletion {
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
}
