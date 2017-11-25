package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
import sake.files.File

/**
 * Command for recursive invocations of sake (as a new process), usually in a different directory.
 */
case object SakeCommand extends ShellCommandBase {

  import Argument._

  val name = "SakeCommand"

  val sakeFile: Argument[File] = Argument.existingFile(
    required = false,
    default  = None,
    description = "The build file. Prepend the directory if NOT in the working directory")

  val directory: Argument[File] = Argument.existingDirectory(
    required = false,
    default  = None,
    description = "The directory to switch to before running sake")

  val targets: Argument[Seq[String]] = Argument.seq[String](
    required = false,
    default  = None,
    description = "Build targets.")

  val args: Argument[Seq[String]] = Argument.seq[String](
    required = false,
    default  = None,
    description = "Any other arguments to pass to sake")

  val supportedArguments: Set[Argument[_]] = Set(
    sakeFile,
    directory,
    targets,
    args
  )

  def sake(args: ArgumentInstance[_]*): Result = run(args)
  def sake(args: Seq[ArgumentInstance[_]]): Result = run(args)

  protected case class SakeTokens (
    targets: Seq[String] = Vector.empty,
    args: Seq[String] = Vector.empty,
    sakeFile: Option[File] = None,
    directory: Option[File] = None) extends ShellCommandBase.TokensBase(Vector.empty, "", true) {

    def appendArgs(newArgs: Seq[String]) = copy(args = args ++ newArgs)
    def appendTargets(newTargets: Seq[String]) = copy(targets = targets ++ newTargets)
    def setSakeFile(file: File) = copy(sakeFile = Some(file))
    def setDirectory(dir: File) = copy(directory = Some(dir))

    def doValidate: (Boolean, String) = (true, "") // nothing to verify!

    override def toSeq: Seq[String] = {
      val f = if (sakeFile  != None) Vector("--file",      sakeFile.get.toString)  else Vector.empty[String]
      val d = if (directory != None) Vector("--directory", directory.get.toString) else Vector.empty[String]
      Vector("sake") ++ f ++ d ++ args ++ targets
    }
  }

  type Tokens = SakeTokens

  protected def defaultTokens: Tokens = SakeTokens()

  protected def handleArgument[V](tokens: Tokens, arg: Argument[V], value: V): Either[String, Tokens] = arg match {
    case `sakeFile`  => Right(tokens.setSakeFile(value.asInstanceOf[File]))
    case `directory` => Right(tokens.setDirectory(value.asInstanceOf[File]))
    case `targets`   => Right(tokens.appendTargets(value.asInstanceOf[Seq[String]]))
    case `args`      => Right(tokens.appendArgs(value.asInstanceOf[Seq[String]]))
    case unknown     => Left(s"Unknown argument instance: argument = $arg, value = $value")
  }
}

