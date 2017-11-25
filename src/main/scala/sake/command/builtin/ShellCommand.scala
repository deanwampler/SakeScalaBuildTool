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

  abstract class TokensBase() {
    /**
     * Extra validation of the tokens. Called by `validate`.
     * @return (true, "") if valid or (false, error_message) if not.
     */
    def doValidate: (Boolean, String)

    /**
     * Convert the tokens into the correct sequence to run as a shell command.
     */
    def toSeq: Seq[String]

    /**
     * Are there no tokens?
     */
    def empty: Boolean

    def validate: (Boolean, String) = {
      val (okay, error) = doValidate
      if (okay == false) (okay, error)
      else if (empty) (false, "Must specify a sequence of tokens")
      else (true, "")
    }
  }

  type Tokens <: TokensBase

  /**
   * Handle the given argument and value, adding to the input tokens, as appropriate.
   * @return either Left(error) or Right(updated_tokens)
   */
  protected def handleArgument[V](tokens: Tokens, arg: Argument[V], value: V): Either[String, Tokens]

  /** Create an empty tokens object of the correct type. */
  protected def defaultTokens: Tokens

  val action: Seq[ArgumentInstance[_]] => Result = args => {
    def tokenize(args2: Seq[ArgumentInstance[_]], tokens: Tokens): Either[String, Tokens] = args2 match {
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
