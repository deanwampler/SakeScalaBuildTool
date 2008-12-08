package sake.util

import org.specs._ 
import sake.Predef._

object ExitSpec extends Specification { 
    "The error method with just a message" should {

        "throw a BuildError with the message" in {
            try {
                Exit.error("message") 
                fail()
            } catch {
                case BuildError(msg, th) =>  msg must be_==("message")
                case _ => fail()
            }
        }
    }

    "The error method with a message and a Throwable" should {
        "throw a BuildError with the message and the throwable" in {
            val ex = new Exception()
            try {
                Exit.error("message", ex) 
                fail()
            } catch {
                case BuildError(msg, th) =>  {
                    msg must be_==("message")
                    th  must be_eq(ex)
                }
                case _ => fail()
            }
        }
    }
}
