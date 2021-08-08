package ammonite.compiler

import scala.tools.nsc.interactive.Global
import scala.util.Try
import scala.io.Source

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import me.shadaj.scalapy.interpreter.CPythonInterpreter

trait Completion {
  val global: Global

  abstract class Run(
    tree: => global.Tree,
    evalClassloader: => ClassLoader,
    allCode: String,
    index: Int
  ) {
    def prefixed: Option[(Int, Seq[(String, Option[String])])]
  }
}

trait ScalaPyCompletion extends Completion {
  import global.{Try => _, _}

  class Run(
    tree: => Tree,
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
                    .Completer(py"globals()")
                    .global_matches(prefixStr)
                    .as[Seq[String]]
                    .filter(_.startsWith(prefixStr))

                  Some(offset, matches.map((_, None)))

                case _ :: _ =>
                  val exprStr = attrs.mkString(".") + "."
                  val text = exprStr + prefixStr
                  val pattern = s"^${exprStr}(${prefixStr}.*)$$".r
                  val matches = py.module("rlcompleter")
                    .Completer(py"globals()")
                    .attr_matches(text)
                    .as[Seq[String]]
                    .map(pattern.findFirstMatchIn(_).map(_.subgroups))
                    .collect { case Some(s :: Nil) => s }

                  Some(offset, matches.map((_, None)))

                case _ => None
              }

            case q"ammonite.$$sess.${TermName(cmd)}.${TermName(v)}" if cmd.startsWith("cmd") =>
              val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
              val instance = cmdClass.getField("MODULE$").get(null)

              val rootValue = cmdClass
                .getDeclaredMethod(v)
                .invoke(instance)
                .asInstanceOf[py.Dynamic]

              Try {
                ScalaPyCompletion.pyAttrMatches(
                  attrs.foldLeft(rootValue)(_.selectDynamic(_)),
                  prefixStr
                ).as[Seq[String]].filter(_.startsWith(prefixStr)).map((_, None))
              }.toOption.map((offset, _))

            case q"ammonite.$$sess.${TermName(cmd)}.instance.${TermName(v)}"
              if cmd.startsWith("cmd") =>
              val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
              val instance = cmdClass.getField("MODULE$").get(null)
              val instanceMethod = cmdClass.getDeclaredMethod("instance")

              val rootValue = evalClassloader
                .loadClass(s"ammonite.$$sess.${cmd}$$Helper")
                .getDeclaredMethod(v)
                .invoke(instanceMethod.invoke(instance))
                .asInstanceOf[py.Dynamic]

              Try {
                ScalaPyCompletion.pyAttrMatches(
                  attrs.foldLeft(rootValue)(_.selectDynamic(_)),
                  prefixStr
                ).as[Seq[String]].filter(_.startsWith(prefixStr)).map((_, None))
              }.toOption.map((offset, _))

            case _ => None
          }

      case _ => None
    }
  }

  object SelectChain {
    def unapply(t: Tree): Option[(Tree, List[String])] = {
      val r = t match {
        case q"${SelectChain(q, attrs)}.${TermName(attrN)}" =>
          Some((q, attrN.toString :: attrs))
        case _ => Some(t, Nil)
      }
      r.collect { case (q, attrs) => (q, attrs.reverse) }
    }
  }

  object SelectDynamicChain {
    def unapply(t: Tree): Option[(Tree, List[String])] = {
      val r = t match {
        case q"${SelectDynamicChain(q, args)}.selectDynamic(${Literal(Constant(argN))})" =>
          Some((q, argN.toString :: args))
        case _ => Some(t, Nil)
      }
      r.collect { case (q, args) => (q, args.reverse) }
    }
  }
}

object ScalaPyCompletion {
  CPythonInterpreter.execManyLines(Source.fromResource("scalapy_completion.py").mkString)

  val pyAttrMatches = py.Dynamic.global.__scalapy_completion_attr_matches
}
