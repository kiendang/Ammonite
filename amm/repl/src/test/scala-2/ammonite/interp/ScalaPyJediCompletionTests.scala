package ammonite.interp

import ammonite.DualTestRepl
import utest._
import ammonite.interp.api.IvyConstructor
import coursierapi.Dependency

object ScalaPyJediCompletionTests extends TestSuite{
  val isTest =
    Option(System.getenv("CI_SCALAPY_TEST")).filter(_.trim.nonEmpty).isDefined &&
      Option(System.getenv("CI_SCALAPYJEDI_TEST")).filter(_.trim.nonEmpty).isDefined

  class Completer{
    val check = new DualTestRepl()
    def apply(caretCode: String,
              cmp: (Set[String]) => Set[String],
              sigs: (Set[String]) => Set[String] = _ => Set()) = {
      val from = caretCode.indexOf("<from>")
      val caretCode0 =
        if (from < 0) caretCode
        else caretCode.replace("<from>", "")
      val cursor = caretCode0.indexOf("<caret>")
      val buf = caretCode0.replace("<caret>", "")

      for (interp <- check.interps) {
        val (index, completions, signatures) = interp.compilerManager.complete(
          cursor,
          interp.frameImports.toString,
          buf
        )
        val left = cmp(completions.toSet)
        assert(left == Set())
        val sigLeft = sigs(signatures.toSet)
        assert(sigLeft == Set())
        assert(from < 0 || index == from)
      }
    }
  }
  def checking[T](code: Option[String] = None)(f: Completer => T) = {
    val c = new Completer

    val sbv = IvyConstructor.scalaBinaryVersion(c.check.scalaVersion)
    c.check.interps.foreach { interp =>
      interp
        .loadIvy(
          Dependency.of("me.shadaj", s"scalapy-core_$sbv", "0.5.0+8-7c7a6042"),
          Dependency.of("ai.kien", s"python-native-libs_$sbv", "0.2.1")
        )
        .foreach { loaded =>
          loaded
            .map(_.toURI.toURL)
            .foreach(jar => interp.headFrame.addClasspath(Seq(jar)))
    }}

    c.check.repls.map(_.run(
      """ai.kien.python.Python().scalapyProperties.get.foreach {
        |  case (k, v) => System.setProperty(k, v)
        |}
      """.stripMargin, 0
    ))

    c.check.repls.map(_.run(
      """import me.shadaj.scalapy.py
        |import me.shadaj.scalapy.py.{PyQuote, SeqConverters}
        |import me.shadaj.scalapy.interpreter.CPythonInterpreter
      """.stripMargin, 1
    ))
    code.map(code => c.check.repls.map(_.run(code, 2)))

    val res = f(c)
    c.check.interps.map(_.compilerManager.shutdownPressy())
    res
  }
  implicit class SetExt[T](s1: Set[T]) {
    def ^(s2: Set[T]): Set[T] = (s1 diff s2) | (s2 diff s1)
  }

  val tests = if (isTest) Tests{
    println("ScalaPyCompletionTests")

    test("py.Dynamic.global") {
      test("shallow") - checking() { complete =>
        complete("""me.shadaj.scalapy.py.<from>Dy<caret>""", Set("Dynamic") -- _)
        complete("""me.shadaj.scalapy.py.Dynamic.global.li<caret>""", Set("list") -- _)
        complete("""py.Dynamic.global.li<caret>""", Set("list") -- _)
      }

      test("nested") - checking(
        Some("""CPythonInterpreter.execManyLines("class Cls(): x = 'string'")""")
      ) { complete =>
        complete("""py.Dynamic.global.Cl<caret>""", Set("Cls") -- _)
        complete("""py.Dynamic.global.Cls.x.<caret>""", Set("upper", "lower") -- _)
      }

      test("assigned") - checking(Some("""val pyGlobal = py.Dynamic.global""")) { complete =>
        complete("""pyGlobal.li<caret>""", Set("list") -- _)
      }

      test("function") - checking(
        Some("""CPythonInterpreter.execManyLines("class Cls(): x = 'string'")""")
      ) { complete =>
        complete("""py.Dynamic.global.range(10).cou<caret>""", Set("count") -- _)
        complete("""py.Dynamic.global.Cls().x.<caret>""", Set("upper", "lower") -- _)
      }
    }

    test("py.Dynamic") {
      test("apply") - checking(Some(
        """val v = py.Dynamic.global.range"""
      )) { complete =>
        complete("""v().cou<caret>""", Set("count") -- _)
        complete("""v(10).cou<caret>""", Set("count") -- _)
      }
      test("shallow") - checking(Some(
        """val v = py"'string'"
          |var vr = py"'string'"
          |def d = py"'string'"
          |lazy val l = py"'string'"
        """.stripMargin
      )) { complete =>
        complete("""v.<caret>""", Set("upper", "lower") -- _)
        complete("""vr.<caret>""", Set("upper", "lower") -- _)
        complete("""v.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""vr.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""v.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""vr.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""d.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""l.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
      }

      test("nested") - checking(Some(
        """class Cls {
          |  val v = py"'string'"
          |  var vr = py"'string'"
          |  def d = py"'string'"
          |  lazy val l = py"'string'"
          |}
          |val inst = new Cls
          |object Obj {
          |  val v = py"'string'"
          |  var vr = py"'string'"
          |  def d = py"'string'"
          |  lazy val l = py"'string'"
          |  val inst = new Cls
          |}
        """.stripMargin
      )) { complete =>
        complete("""Obj.v.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.vr.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.v.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.vr.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.v.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""Obj.vr.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""Obj.d.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.l.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.inst.v.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.vr.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.v.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.vr.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.v.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""Obj.inst.vr.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""Obj.inst.d.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.inst.l.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""inst.v.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.vr.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.v.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""inst.vr.upper().<caret>""", Set("upper", "lower") -- _)
        complete("""inst.v.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""inst.vr.rjust(10, "*").cap<caret>""", Set("capitalize") -- _)
        complete("""inst.d.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""inst.l.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
      }

      test("nestedpython") - checking(Some(
        """CPythonInterpreter.execManyLines("class Cls(): x = 'string'")
          |class Cls {
          |  val v = py"Cls"
          |  var vr = py"Cls"
          |  def d = py"Cls"
          |  lazy val l = py"Cls"
          |}
          |val inst = new Cls
          |object Obj {
          |  val v = py"Cls"
          |  var vr = py"Cls"
          |  def d = py"Cls"
          |  lazy val l = py"Cls"
          |  val inst = new Cls
          |}
        """.stripMargin
      )) { complete =>
        complete("""Obj.v.x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.vr.x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.v().x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.vr().x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.d.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.l.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.inst.v.x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.vr.x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.v().x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.vr().x.<caret>""", Set("upper", "lower") -- _)
        complete("""Obj.inst.d.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""Obj.inst.l.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""inst.v.x.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.vr.x.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.v().x.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.vr().x.<caret>""", Set("upper", "lower") -- _)
        complete("""inst.d.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
        complete("""inst.l.x.<caret>""", s => if (s.isEmpty) Set.empty else Set(""))
      }
    }
  } else Tests()
}
