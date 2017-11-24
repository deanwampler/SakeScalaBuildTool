package sake.command.builtin

import org.scalatest._
import org.scalatest.Matchers._
// Yes, this object and file are deliberately misspelled so they aren't picked up by the normal "spec" target!
object FailingSpek extends FreeSpec {
    "1 + 1" should {
        "fail if the result is expected to be 3" in {
            (1 + 1) shouldEqual 3
        }
    }
}
