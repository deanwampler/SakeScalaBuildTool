package sake.command

import sake.files.{File, Path}

/**
 * Define and run an arbitrary shell commands. You pass the entire
 * command-line string to the constructor.
 */
case object ShellCommand extends Command {

  import Command._

  val name = "ShellCommand"

  val commandStringArg: Argument[String] = Argument.nonemptyString(
    required = false,
    default  = None,
    description = "Holds the whole command string")

  val commandTokensArg: Argument[Seq[String]] = Argument.nonemptySeq[String](
    required = false,
    default  = None,
    description = "Holds the whole command string")

  val supportedArguments: Set[Argument[_]] = Set(commandStringArg, commandTokensArg)

  def sh(command: String): Result = commandStringArg(command) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  def sh(commandTokens: Seq[String]): Result = commandTokensArg(commandTokens) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  import scala.sys.process._

  val action: Seq[ArgumentInstance[_]] => Result = {
    case ArgumentInstance(commandStringArg, commandString: String) +: Nil =>
      val exitCode = commandString.!
      if (exitCode == 0) Passed.default
      else Failed(exitCode, "Shell command failed: $commandString")
    case ArgumentInstance(commandTokensArg, commandTokens: Seq[_]) +: Nil =>
      val tokens = commandTokens.map(_.toString)      // force "_" to be String
      val exitCode = tokens.!
      if (exitCode == 0) Passed.default
      else Failed(exitCode, s"Shell command failed: ${tokens.mkString(" ")}")
    case Nil =>
      Failed(1, "No command string or tokens specified!")
    case seq =>
      Failed(1, s"Can't specify the command using both a string and a sequence of tokens: ${seq.mkString(", ")}")
  }
}
