package sake.command

import sake.util._

abstract case class Result() {
    val success: Boolean

    /** 
     * Allow the user to specify an optional closure after a command that is effectively a
     * post-processing hook. The Result of the command's execution is passed as the one argument.
     * However, because of parsing issues and the need to handle failures at a place where we "see"
     * them, failures are handled before the closure is invoked. Hence, this hook is only useful for
     * post-processing successful results. 
     */
    def and(postAction: (Result) => Result) = postAction(this)
}

case class Passed[R](result: Option[R], message: Option[String]) extends Result {
    override val success = true
    
    def this(result: Option[R]) = this(result, None)
    def this() = this(None)
    
    override def toString() = ResultString.asString(result, message)
}

case class Failed[R](result: Option[R], message: Option[String]) extends Result {
    override val success = false
    
    def this(result: Option[R]) = this(result, None)
    def this() = this(None)
    
    override def toString() = ResultString.asString(result, message)
}

object ResultString {
    def asString[R](result: Option[R], message: Option[R]) = {
        var s = new StringBuilder()
        s.append("(status: ")
        result match {
            case None => s.append("?")
            case Some(r) => s.append(r.toString())
        }
        message match {
            case None => 
            case Some(r) => s.append(", message: ").append(r.toString())
        }
        s.append(")").toString()
    }
}