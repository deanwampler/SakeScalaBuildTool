package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
import sake.util.Log

/**
 * Log a string at the specified level.
 * You can also use "println". use EchoCommand if you want the output to be logged consistently
 * with the rest of the build output.
 */
case class EchoCommand(level: Log.Level.Value = Log.Level.Info) extends Command {

  import Argument._

  val name = "EchoCommand"

  val echoStringArg: Argument[String] = Argument.nonemptyString(
    required = false,
    default  = None,
    description = "Holds the whole echo string")

  val echoTokensArg: Argument[Seq[String]] = Argument.nonemptySeq[String](
    required = false,
    default  = None,
    description = "Holds the whole echo string as tokens")

  val supportedArguments: Set[Argument[_]] = Set(echoStringArg, echoTokensArg)

  def echo(echo: String): Result = echoStringArg(echo) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  def echo(echoTokens: Seq[String]): Result = echoTokensArg(echoTokens) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  import scala.sys.process._

  val action: Seq[ArgumentInstance[_]] => Result = {
    case ArgumentInstance(echoStringArg, echoString: String) +: Nil =>
      Log.default(level, echoString)
      Passed.default
    case ArgumentInstance(echoTokensArg, echoTokens: Seq[_]) +: Nil =>
      val tokens = echoTokens.map(_.toString)      // force "_" to be String
      Log.default(level, tokens.mkString(" "))
      Passed.default
    case Nil =>
      Failed(1, "No echo string or tokens specified!")
    case seq =>
      Failed(1, s"Can't specify echo arguments using both a string and a sequence of tokens: ${seq.mkString(", ")}")
  }
}

