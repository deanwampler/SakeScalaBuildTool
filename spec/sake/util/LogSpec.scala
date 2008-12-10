package sake.util

import org.specs._ 
import java.io.{PrintStream, ByteArrayOutputStream}

object LogSpec extends Specification { 

    def checkDefaultLog(l: Log) = {
        l.threshold = Level.Warn
        l.out = Console.out
        val s = new Log().messageFormatter(Level.Info, "This is a message")
        s.contains("Info") must be_==(true)
        s.contains("This is a message") must be_==(true)
    }
    
    "Constructing a Log" should {
        
        "default to Warn, Console.out and a simple string formatter" in {
            checkDefaultLog(new Log())
        }
        
        "accept a threshold only" in {
            new Log(Level.Fatal).threshold must be_==(Level.Fatal)
        }

        "accept a threshold and output stream" in {
            val newStream = new PrintStream(new ByteArrayOutputStream())
            val l = new Log(Level.Fatal, newStream)
            l.threshold must be_==(Level.Fatal)
            (l.out eq newStream) must be_==(true)
        }

        "accept a threshold, output stream, and message formatter" in {
            val newStream = new PrintStream(new ByteArrayOutputStream())
            val messageFormatter = (l:Level.Value, m:String) => String.format("%s : %s : %s", m, l, m)
            val l = new Log(Level.Fatal, newStream, messageFormatter)
            l.threshold must be_==(Level.Fatal)
            (l.out eq newStream) must be_==(true)
            l.messageFormatter(Level.Notice, "msg") must be_==("msg : Notice : msg")
        }
    }

    "The logging threshold" should {

        "default to Warn" in {
            checkDefaultLog(new Log())
        }

        "be changeable to another Level" in {
            val l = new Log()
            l.threshold = Level.Fatal
            l.threshold must be_==(Level.Fatal)
        }
    }
    
    "The output stream" should {

        "default to Console.out" in {
            checkDefaultLog(new Log())
        }

        "be changeable to a new stream" in {
            val l = new Log()
            val newStream = new PrintStream(new ByteArrayOutputStream())
            l.out = newStream
            (l.out eq newStream) must be_==(true)
        }
    }
    
    "The messageFormatter" should {

        "default to the logging level and input message" in {
            checkDefaultLog(new Log())
        }

        "be changeable to a new formatter function taking the level and the message as parameters" in {
            val l = new Log()
            l.messageFormatter = (l, m) => String.format("%s : %s : %s", m, l, m)
            l.messageFormatter(Level.Notice, "msg") must be_==("msg : Notice : msg")
        }
    }
    
    "apply()" should {
        
        "do no logging if the logging level is below the threshold" in {
            val newStream = new PrintStream(new ByteArrayOutputStream())
            var called = false
            val messageFormatter = (l:Level.Value, m:String) => { called = true; "" }
            val l = new Log(Level.Warn, newStream, messageFormatter)
            for (level <- Level) {
                called = false
                l(level, "")
                called must be_==(level >= Level.Warn)
            }
        }
    }
}
