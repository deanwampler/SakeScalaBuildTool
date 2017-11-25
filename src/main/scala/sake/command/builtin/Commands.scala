package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
import sake.files.{File, JavaFilesFinder, Path}
import sake.util.{Exit, Log}

trait Commands {

  var classpath = Path()

  val files = JavaFilesFinder

  /** Make one or more directories, but fail if parents don't exist. */
  def mkdir(path: String, paths: String*): Result = mkdir(path +: paths)

  /** Make one or more directories, but fail if parents don't exist. */
  def mkdir(paths: Seq[String]): Result = recurse(paths, "Could not create directory %s.") { path =>
    val dir = File(path)
    dir.exists || dir.mkdir
  }

  /** Make one or more directories, including the parents, if necessary. */
  def mkdirs(path: String, paths: String*): Result = mkdirs(path +: paths)

  /** Make one or more directories, including the parents, if necessary. */
  def mkdirs(paths: Seq[String]): Result = recurse(paths, "Could not create directory %s, including parents.") { path =>
    val dir = File(path)
    dir.exists || dir.mkdirs
  }

  /** Delete one or more paths, but not recursively. */
  def delete(path: String, paths: String*): Result = delete(path +: paths)

  /** Delete one or more paths, but not recursively. */
  def delete(paths: Seq[String]): Result = recurse(paths, "Could not delete directory %s.") { path =>
    val dir = File(path)
    dir.exists == false || dir.delete
  }

  /** Delete one or more paths recursively. */
  def deleteRecursively(path: String, paths: String*): Result = deleteRecursively(path +: paths)

  /** Delete one or more paths recursively. */
  def deleteRecursively(paths: Seq[String]): Result = recurse(paths, "Could not delete directory %s recursively.") { path =>
    val dir = File(path)
    dir.exists == false || dir.deleteRecursively
  }

  // Commands for logging ("echoing") messages.

  /** Log an info message. It doesn't return a Result. */
  def info(  message: String): Unit = Log.info(message)

  /** Log a notice message. It doesn't return a Result. */
  def notice(message: String): Unit = Log.notice(message)

  /** Log a warning message. It doesn't return a Result. */
  def warn(  message: String): Unit = Log.warn(message)

  /** Log an error message. It doesn't return a Result. */
  def error( message: String): Unit = Log.error(message)

  /** Log a fatal message. It doesn't return a Result. */
  def fatal( message: String): Unit = Log.fatal(message)

  /** Fail immediately with a message. */
  def fail(message: String = "Build failed!"): Unit = Exit.error(message)

  /**
   * Command for recursive invocation of sake (as a new process), usually in a different directory.
   */
  def sake(arg: SakeCommand.ArgumentInstance[_], args: SakeCommand.ArgumentInstance[_]*): Result = SakeCommand.sake(arg +: args)

  /**
   * Command for recursive invocation of sake (as a new process), usually in a different directory.
   */
  def sake(args: Seq[SakeCommand.ArgumentInstance[_]]): Result = SakeCommand.sake(args)

  /**
   * Command for invocation of a Java command.
   */
  def java(arg: JavaCommand.ArgumentInstance[_], args: JavaCommand.ArgumentInstance[_]*): Result = JavaCommand.java(arg +: args)

  /**
   * Command for invocation of a Java command.
   */
  def java(args: Seq[JavaCommand.ArgumentInstance[_]]): Result = JavaCommand.java(args)


  protected def recurse[T](seq: Seq[T], errorMessage: String)(predicate: T => Boolean): Result = {
    def r(seq2: Seq[T]): Result = seq2 match {
      case Nil => Passed.default
      case head +: tail => if(predicate(head)) r(tail) else Failed(1, errorMessage.format(head.toString))
    }
    r(seq)
  }
}
