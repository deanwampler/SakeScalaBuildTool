package sake.command.builtin

import org.specs._ 

// Yes, this object and file are deliberately misspelled so they aren't picked up by the normal "spec" target!
object PassingSpek extends Specification {
    "1 + 1" should {
        "pass if the result is expected to be 2" in {
            (1 + 1) mustEqual 2
        }
    }
}
