package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
import sake.files.{File, Path}
import scala.sys.process._

/**
 * Trait shared by commands implemented using shell command invocations, where
 * the command it built up using `Arguments`. For running strings as shell commands,
 * the Scala process API is supported.
 */
trait ShellCommand extends Command {

  import Argument._

  abstract class TokensBase(tokens: Seq[String]) {
    /**
     * Extra validation of the tokens. Called by `validate`.
     * @return (true, "") if valid or (false, error_message) if not.
     */
    def doValidate: (Boolean, String)

    /**
     * Convert the tokens into the correct sequence to run as a shell command.
     * The default implementation just uses the tokens as is. Subclasses can
     * override this behavior.
     */
    def toSeq: Seq[String] = tokens

    def validate: (Boolean, String) = {
      val (okay, error) = doValidate
      if (okay == false) (okay, error)
      else if (tokens.size == 0) (false, "Must specify a sequence of tokens")
      else (true, "")
    }
  }

  /**
   * Handle the given argument and value, adding to the input tokens, as appropriate.
   * @return either Left(error) or Right(updated_tokens)
   */
  protected def handleArgument[T <: TokensBase, V](tokens: Tokens, arg: Argument[V], value: V): Either[String, Tokens]

  /** Create an empty tokens object of the correct type. */
  protected def defaultTokens[T <: TokensBase]: T

  val action: Seq[ArgumentInstance[_]] => Result = args => {
    def tokenize[T <: TokensBase](args2: Seq[ArgumentInstance[_]], tokens: T): Either[String, T] = args2 match {
      case Nil => Right(tokens)
      case ArgumentInstance(argument, value) +: tail =>
        handleArgument(tokens, argument, value) match {
          case Right(tokens) => tokenize(tail, tokens)
          case error => error
        }
    }

    tokenize(args, defaultTokens) match {
      case Left(error) => Failed(1, error)
      case Right(tokens) =>
        tokens.validate match {
          case (false, error) => Failed(1, s"$error: args = ${args.mkString(" ")}")
          case (true, _) =>
            val toks = tokens.toSeq
            exitCodeToResult(execute(toks), toks.mkString(" "))
        }
    }
  }

  /* A separate method is used for actual execution so that it can be overridden easily in test doubles. */
  protected def execute(tokens: Seq[String]): Int = tokens.!

  protected def exitCodeToResult(exitCode: Int, command: String): Result = {
    if (exitCode == 0) Passed.default
    else Failed(exitCode, "Shell command failed: $value")
  }
}

/**
 * Define and run an arbitrary shell commands. You pass the entire
 * command-line string to the constructor.
 */
case object ShellCommand extends ShellCommandBase {

  protected case class ShellCommandTokens(
    tokens: Seq[String] = Vector.empty,
    commandString: String = "",
    useTokens: Boolean = false) extends ShellCommandBase.TokensBase(tokens, commandString, useTokens) {

    // Nothing more to do.
    def doValidate: (Boolean, String) = (true, "")
  }

  type Tokens = ShellCommandTokens

  import Argument._

  val name = "ShellCommand"

  val commandString: Argument[String] = Argument.nonemptyString(
    required = false,
    default  = None,
    description = "Holds the whole command string")

  val commandTokens: Argument[Seq[String]] = Argument.nonemptySeq[String](
    required = false,
    default  = None,
    description = "Holds the whole command string")

  val supportedArguments: Set[Argument[_]] = Set(commandString, commandTokens)

  def sh(command: String): Result = commandString(command) match {
    case Left(error) => Failed(1, error.toString)
    case Right(argInstance) => run(Seq(argInstance.asInstanceOf[ArgumentInstance[String]]))
  }

  def sh(command: Seq[String]): Result = commandTokens(command) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance.asInstanceOf[Seq[ArgumentInstance[String]]])
  }

  protected def defaultTokens: Tokens = ShellCommandTokens()

  protected def handleArgument[V](tokens: Tokens, arg: Argument[V], value: V): Either[String, Tokens] = arg match {
    case ArgumentInstance(commandString, command: String) =>
      Right(tokens.copy(commandString = command, useTokens = false))
    case ArgumentInstance(commandTokens, tokens: Seq[_]) =>
      val stringTokens = tokens.map(_.toString)      // force "_" to be String
      Right(tokens.copy(tokens = stringTokens, useTokens = true))
    case unknown =>
      Left(s"Unknown command argument: argument = $arg, value = $value")
  }
}
