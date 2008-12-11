package sake.util

import org.specs._
import sake.environment._

// Uses the full path to Path to avoid collisions with Spec's own FileMatchers.Path.
object PathSpec extends Specification { 

    "The Path delim" should {
        
        "equal the system path separator" in {
             sake.util.Path.delim must be_==(new Environment().pathSeparator)
        }        
    }
    
    "apply" should {
        "convert a sequence of Any's into a delim-separated string" in {
            sake.util.Path(List(1, 2.3, "boo", 'boo, List("x", "y"), ("a", 'b))) must 
                be_==("1:2.3:boo:'boo:List(x, y):(a,'b)")
        }
    }
}