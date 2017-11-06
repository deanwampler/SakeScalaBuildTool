package sake.command

import org.specs._ 

object ResultSpec extends Specification {
    
    import sake.command._
    import sake.util._
    
    "A successful result" should {
        "be true" in {
            (new Passed()).success mustEqual true
        }
    }
    
    "A failed result" should {
        "be false" in {
            (new Failed()).success mustEqual false
        }
    }
    
    "A result" should {
        "have an optional result" in {
            (new Passed()).result mustEqual None
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
            (new Passed()).message mustEqual None
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
            r2.success mustEqual false
            (r1 eq r2) mustEqual false
        }
    }
    
    "ResultString.asString method" should {
        "formats a user-friendly message" in {
            val p1 = new Passed()
            ResultString.asString(p1.result, p1.message) mustEqual "(status: ?)"
            val p2 = new Passed(None)
            ResultString.asString(p2.result, p2.message) mustEqual "(status: ?)"
            val p3 = new Passed(None, None)
            ResultString.asString(p3.result, p3.message) mustEqual "(status: ?)"
            val p4 = new Passed(Some(0))
            ResultString.asString(p4.result, p4.message) mustEqual "(status: 0)"
            val p5 = new Passed(Some(0), None)
            ResultString.asString(p5.result, p5.message) mustEqual "(status: 0)"
            val p6 = new Passed(Some(0), Some("message1"))
            ResultString.asString(p6.result, p6.message) mustEqual "(status: 0, message: message1)"

            val f1 = new Failed()
            ResultString.asString(f1.result, f1.message) mustEqual "(status: ?)"
            val f2 = new Failed(None)
            ResultString.asString(f2.result, f2.message) mustEqual "(status: ?)"
            val f3 = new Failed(None, None)
            ResultString.asString(f3.result, f3.message) mustEqual "(status: ?)"
            val f4 = new Failed(Some(1))
            ResultString.asString(f4.result, f4.message) mustEqual "(status: 1)"
            val f5 = new Failed(Some(1), None)
            ResultString.asString(f5.result, f5.message) mustEqual "(status: 1)"
            val f6 = new Failed(Some(1), Some("message1"))
            ResultString.asString(f6.result, f6.message) mustEqual "(status: 1, message: message1)"
        }
    }
    
}