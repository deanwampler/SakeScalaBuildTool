package sake.util

import org.specs._ 
import sake.Predef._

object ExitSpec extends Specification { 
    "The error method" should {

        "throw a BuildError with the message passed as the first parameter" in {
            try {
                Exit.error("message") 
                fail()
            } catch {
                case BuildError(msg) =>  msg must be_==("message")
                case _ => fail()
            }
        }
    }
}
