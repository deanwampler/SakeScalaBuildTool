package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
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
   * Command for recursive invocation of sake (as a new process), usually in a different directory.
   */
  def sake(args: SakeCommand.ArgumentInstance[_]*) = SakeCommand.sake(args)
  def sake(args: Seq[SakeCommand.ArgumentInstance[_]]) = SakeCommand.sake(args)

  /**
   * Command for invocation of a Java command.
   */
  def java(args: JavaCommand.ArgumentInstance[_]*) = JavaCommand.java(args)
  def java(args: Seq[JavaCommand.ArgumentInstance[_]]) = JavaCommand.java(args)

  /**
   * Use "sh" for invoking a shell command with a command string.
   */
  def sh(command: String) = ShellCommand.sh(command)

  val echo = new EchoCommand()

  // val scala  = new JavaCommand("scala")
  // val scalac = new JavaCommand("scalac", 'files -> ".")

  // val specs  = new SpecCommand()

  // val java   = new JavaCommand("java")
  // val javac  = new JavaCommand("javac", 'files -> ".")
}
