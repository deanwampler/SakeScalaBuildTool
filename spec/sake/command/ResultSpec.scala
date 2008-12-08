package sake.command

import org.specs._ 

object ResultSpec extends Specification {
    
    import sake.command._
    import sake.util._
    
    "A successful result" should {
        "be true" in {
            (new Passed()).success must be_==(true)
        }
    }
    
    "A failed result" should {
        "be false" in {
            (new Failed()).success must be_==(false)
        }
    }
    
    "A result" should {
        "have an optional result" in {
            (new Passed()).result must be_==(None)
            (new Passed(Some("success"))).result match {
                case Some("success") => 
                case _ => fail()
            } 
            (new Passed(Some(List("success")))).result match {
                case Some(List("success")) => 
                case _ => fail()
            }
        }
        "have an optional message" in {
            (new Passed()).message must be_==(None)
            (new Passed(None, Some("success"))).message match {
                case Some("success") => 
                case _ => fail()
            } 
        }
        "accept an optional closure for post-processing of the event" in {
            val r1 = new Passed() 
            val r2 = r1 and { result =>
                new Failed()
            }
            r2.success must be_==(false)
            (r1 eq r2) must be_==(false)
        }
    }
}