package sake.command

import sake.util._

abstract class Result {
    val success: Boolean

    /** 
     * Allow the user to specify an optional closure after a command that is effectively a
     * post-processing hook. The Result of the command's execution is passed as the one argument.
     */
    def and(postAction: (Result) => Result) = postAction(this)
}

case class Passed[R](result: Option[R], message: Option[String]) extends Result {
    override val success = true
    
    def this(result: Option[R]) = this(result, None)
    def this() = this(None)
}

case class Failed[R](result: Option[R], message: Option[String]) extends Result {
    override val success = false
    
    def this(result: Option[R]) = this(result, None)
    def this() = this(None)
}
