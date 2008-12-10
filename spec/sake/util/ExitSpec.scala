package sake.util

import org.specs._ 

object ExitSpec extends Specification { 
    val oldLog = Log.log
    
    doBeforeSpec {
        // Suppress stdout/stderr output...
        import java.io.{PrintStream, ByteArrayOutputStream}
        val newStream = new PrintStream(new ByteArrayOutputStream())
        Log.log = new Log(Level.Warn, newStream)
    }
    
    doAfterSpec {
        Log.log = oldLog
    }
    
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
                    (th eq ex) must be_==(true)
                }
                case _ => fail()
            }
        }
    }
}
