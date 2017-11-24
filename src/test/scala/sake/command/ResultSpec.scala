package sake.command

import org.scalatest._
import org.scalatest.Matchers._

object ResultSpec extends FreeSpec {

    import sake.command._
    import sake.util._

    "A successful result" should {
        "be true" in {
            (Passed()).passed shouldEqual true
        }
    }

    "A failed result" should {
        "be false" in {
            (Failed()).passed shouldEqual false
        }
    }

    "A result" should {
        "have an optional result" in {
            (Passed()).result shouldEqual None
            (Passed(Some("success"))).result match {
                case Some("success") =>
                case _ => fail()
            }
            (Passed(Some(List("success")))).result match {
                case Some(List("success")) =>
                case _ => fail()
            }
        }
        "have an optional message" in {
            (Passed()).message shouldEqual None
            (Passed(None, Some("success"))).message match {
                case Some("success") =>
                case _ => fail()
            }
        }
        "accept an optional closure for post-processing of the event" in {
            val r1 = Passed()
            val r2 = r1 and { result =>
                Failed()
            }
            r2.passed shouldEqual false
            (r1 eq r2) shouldEqual false
        }
    }

    "ResultString.asString method" should {
        "formats a user-friendly message" in {
            val p1 = Passed()
            ResultString.asString(p1.result, p1.message) shouldEqual "(status: ?)"
            val p2 = Passed(None)
            ResultString.asString(p2.result, p2.message) shouldEqual "(status: ?)"
            val p3 = Passed(None, None)
            ResultString.asString(p3.result, p3.message) shouldEqual "(status: ?)"
            val p4 = Passed(Some(0))
            ResultString.asString(p4.result, p4.message) shouldEqual "(status: 0)"
            val p5 = Passed(Some(0), None)
            ResultString.asString(p5.result, p5.message) shouldEqual "(status: 0)"
            val p6 = Passed(Some(0), Some("message1"))
            ResultString.asString(p6.result, p6.message) shouldEqual "(status: 0, message: message1)"

            val f1 = Failed()
            ResultString.asString(f1.result, f1.message) shouldEqual "(status: ?)"
            val f2 = Failed(None)
            ResultString.asString(f2.result, f2.message) shouldEqual "(status: ?)"
            val f3 = Failed(None, None)
            ResultString.asString(f3.result, f3.message) shouldEqual "(status: ?)"
            val f4 = Failed(Some(1))
            ResultString.asString(f4.result, f4.message) shouldEqual "(status: 1)"
            val f5 = Failed(Some(1), None)
            ResultString.asString(f5.result, f5.message) shouldEqual "(status: 1)"
            val f6 = Failed(Some(1), Some("message1"))
            ResultString.asString(f6.result, f6.message) shouldEqual "(status: 1, message: message1)"
        }
    }

}
