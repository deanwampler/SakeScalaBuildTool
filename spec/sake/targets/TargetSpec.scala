package sake

import org.specs._ 

object TargetSpec extends Specification {
    
    import sake.targets._

    "A new Target created with just a name" should {
        "have the same name" in {
            (new Target('t)).name must be_==('t)
        }

        "have no dependencies" in {
            (new Target('t)).dependencies must be_==(Nil)
        }
    }
    
    "A new Target created with a name and a list of dependencies" should {
        "have the same name" in {
            (new Target('t, Nil)).name must be_==('t)
        }

        "have the same dependency list" in {
            (new Target('t, Nil)).dependencies must be_==(Nil)
            (new Target('t, List('d1))).dependencies must be_==(List('d1))
            (new Target('t, List('d1, 'd2))).dependencies must be_==(List('d1, 'd2))
        }
    }
    
    "A new Target created with a name, a list of dependencies, and an action" should {
        "have the same name" in {
            (new Target('t, Nil, {})).name must be_==('t)
        }

        "have the same dependency list" in {
            (new Target('t, Nil, {})).dependencies must be_==(Nil)
            (new Target('t, List('d1), {})).dependencies must be_==(List('d1))
            (new Target('t, List('d1, 'd2), {})).dependencies must be_==(List('d1, 'd2))
        }

        "have the same action" in {
            var success = false
            val t = new Target('t, List('d1), { success = true })
            t.build()
            success must be_==(true)
        }
    }
    
    "Calling build() on the Target" should {
        "invoke the Target's build action" in {
            var success = false
            val t = new Target('t, List('d1), { success = true })
            t.build()
            success must be_==(true)
        }
    }
}