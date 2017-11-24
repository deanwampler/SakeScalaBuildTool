package sake.command

import sake.util._
import sake.target.Target

sealed trait Result {
  val exitCode: Int
  val message: String

  def passed: Boolean

  /**
   * Sequence actions. If this result is successful, print its message and
   * proceed to the next action. Otherwise, return this result.
   */
  def and(nextAction: => Result) =
    if (exitCode == 0) {
      Log.log.info(message)
      nextAction
    } else this
}

case class Passed(message: String = "") extends Result {
  val exitCode: Int = 0
  def passed: Boolean = true
}

object Passed {
  val default = Passed("")
}

case class Failed(exitCode: Int = 1, message: String = "", cause: Option[Throwable] = None) extends Result {
  def passed: Boolean = false
}

object Failed {
  def exception(message: String, cause: Throwable) = Failed(255, s"""FAILED: ${message} (cause: $cause)""", Some(cause))
}
