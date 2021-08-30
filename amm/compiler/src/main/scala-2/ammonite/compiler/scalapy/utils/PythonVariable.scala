package ammonite.compiler.scalapy.utils

import scala.collection.compat.immutable.LazyList
import scala.util.Random

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

trait PythonVariable {
  def namespace: py.Dynamic

  def randomName(length: Int = 5): String =
    "__scalapy" + Random.alphanumeric.take(length).mkString

  def newRandomName(tries: Int = 10, length: Int = 5): String = {
    val generated = (1 to tries)
      .to(LazyList)
      .map(_ => randomName(length))
      .find(!exists(_))

    generated match {
      case Some(v) => v
      case None =>
        throw new Exception(s"Could not find a new random variable name in $tries tries")
    }
  }

  def exists(name: String): Boolean = py"$name in $namespace".as[Boolean]

  def delete(name: String): Unit = namespace.bracketDelete(name)

  def update(name: String, newValue: py.Any): Unit = namespace.bracketUpdate(name, newValue)
}
