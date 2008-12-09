package sake

import org.specs._ 

object TargetSpec extends Specification {
    
    import sake.target._
    import sake.util._
    
    def checkDependencies(t: Target, expected: List[Symbol]) =
            t.dependencies must be_==(expected)


    "A new Target created with just a name" should {
        "have the same name" in {
            (Target('t)).name must be_==('t)
        }

        "have no dependencies" in {
            checkDependencies(Target('t), Nil)
        }
    }
    
    "A new Target created with a name and a list of dependencies" should {
        "have the same name" in {
            (Target('t, Nil)).name must be_==('t)
        }

        "have the same dependency list" in {
            checkDependencies(Target('t, Nil), Nil)
            checkDependencies(Target('t, List('d1)), List('d1))
            checkDependencies(Target('t, List('d1, 'd2)), List('d1, 'd2))
        }
    }
    
    "A new Target created with a name, a list of dependencies, and an action" should {
        "have the same name" in {
            (Target('t, Nil, {})).name must be_==('t)
        }

        "have the same dependency list" in {
            checkDependencies(Target('t, Nil, {}), Nil)
            checkDependencies(Target('t, List('d1), {}), List('d1))
            checkDependencies(Target('t, List('d1, 'd2), {}), List('d1, 'd2))
        }

        "have the same action" in {
            var success = false
            val t = Target('t, List('d1), { success = true })
            t.build()
            success must be_==(true)
        }
    }
    
    "Calling build() on the Target" should {
        "invoke the Target's build action" in {
            var success = false
            val t = Target('t, List('d1), { success = true })
            t.build()
            success must be_==(true)
        }
    }
    
    "Merging targets when they don't have the same name" should {
        "Throw a BuildError" in {
            Target.merge(Target('t1), Target('t2)) must throwA[BuildError]
        }
    }
    
    "Merging targets when neither has dependencies nor an action" should {
        "return a target with the same name" in {
            Target.merge(Target('t1), Target('t1)).name must be_==('t1)
        }
    }
}