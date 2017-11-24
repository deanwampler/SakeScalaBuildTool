package sake.command.builtin

import sake.command._
import sake.files.{File, JavaFilesFinder, Path}
import sake.util.Exit

trait Commands {

  var classpath = Path()

  val files = JavaFilesFinder

  def mkdirs(paths: String*): Unit = mkdirs(paths.toList)

  def mkdirs(paths: List[String]): Unit = paths foreach { path =>
    val dir = makeFile(path)
    if (dir.exists == false && dir.mkdirs == false)
      Exit.error("Could not create directory \""+path+"\".")
  }

  def mkdir(path: String) = mkdirs(path)

  def delete(paths: String*): Unit = delete(paths.toList)

  def delete(paths: List[String]): Unit = paths foreach { path =>
    val file = makeFile(path)
    if (file.exists && file.delete == false)
      Exit.error("Could not delete \""+path+"\".")
  }

  def deleteRecursively(paths: String*): Unit = deleteRecursively(paths.toList)

  def deleteRecursively(paths: List[String]): Unit = paths foreach { path =>
    val file = makeFile(path)
    if (file.exists && file.deleteRecursively == false)
      Exit.error("Could not delete \""+path+"\" recursively.")
  }

  def fail(): Unit = fail("Build failed!")

  def fail(message: String): Unit = Exit.error(message)

  protected def makeFile(path: String) = File(path)

  /**
   * Command for recursive invocations of sake (as a new process), usually in a different directory.
   */
  val sakecmd = new SakeCommand()

  /**
   * Use "sh" for invoking a shell command with a command string.
   */
  def sh(command: String) = ShellCommand(command)

  val echo = new EchoCommand()

  val scala  = new JVMCommand("scala")
  val scalac = new JVMCommand("scalac", 'files -> ".")

  // val specs  = new SpecCommand()

  val java   = new JVMCommand("java")
  val javac  = new JVMCommand("javac", 'files -> ".")
}
