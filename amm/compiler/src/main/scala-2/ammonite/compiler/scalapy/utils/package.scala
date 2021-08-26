package ammonite.compiler.scalapy

package object utils {
  def getDeclaredValueDefaultWrapper(cmd: String, term: String, evalClassloader: => ClassLoader) = {
    val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
    val instance = cmdClass.getField("MODULE$").get(null)

    cmdClass
      .getDeclaredMethod(term)
      .invoke(instance)
  }

  def getDeclaredValueClassWrapper(cmd: String, term: String, evalClassloader: => ClassLoader) = {
    val cmdClass = evalClassloader.loadClass(s"ammonite.$$sess.${cmd}$$")
    val instance = cmdClass.getField("MODULE$").get(null)
    val instanceMethod = cmdClass.getDeclaredMethod("instance")

    evalClassloader
      .loadClass(s"ammonite.$$sess.${cmd}$$Helper")
      .getDeclaredMethod(term)
      .invoke(instanceMethod.invoke(instance))
  }
}
