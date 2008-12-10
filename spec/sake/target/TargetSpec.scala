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
    
    "Merging targets" should {
        "when they don't have the same name" in {
            "Throw a BuildError" in {
                Target.merge(Target('t1), Target('t2)) must throwA[BuildError]
            }
        }
    
        "when neither has an action" in {
            "when neither has dependencies" in {
                "return a target with the same name" in {
                    val t = Target.merge(Target('t1), Target('t1))
                    t.name must be_==('t1)
                    t.dependencies.size must be_==(0)
                }
            }

            "when the first has dependencies" in {
                "return a target with the dependencies from the first" in {
                    val t = Target.merge(Target('t1, List('d1, 'd2)), Target('t1))
                    t.name must be_==('t1)
                    t.dependencies must be_==(List('d1, 'd2))
                }
            }

            "when the second has dependencies" in {
                "return a target with the dependencies from the second" in {
                    val t = Target.merge(Target('t1), Target('t1, List('d1, 'd2)))
                    t.name must be_==('t1)
                    t.dependencies must be_==(List('d1, 'd2))
                }
            }

            "when both have dependencies" in {
                "return a target with the merged dependencies with duplicates removed" in {
                    val t = Target.merge(Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5)), 
                                         Target('t1, List('d2, 'd4, 'd6)))
                    t.name must be_==('t1)
                    t.dependencies must be_==(List('d1, 'd2, 'd3, 'd4, 'd5, 'd6))
                }
            }
        }
        
        "when the first has an action" in {
            "return the first target with the same name" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5), {})
                val t2 = Target('t1, List('d2, 'd4, 'd6))
                t1.isInstanceOf[TargetWithAction] must be_==(true)
                val t  = Target.merge(t1, t2)
                (t eq t1) must be_==(true)
                (t eq t2) must be_==(false)
                t.name must be_==('t1)
            }
            
            """return the first target with the merged dependencies with duplicates removed, 
               with preference to the first targets dependency list""" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5), {})
                val t2 = Target('t1, List('d2, 'd4, 'd6))
                val t  = Target.merge(t1, t2)
                (t eq t1) must be_==(true)
                (t eq t2) must be_==(false)
                t.name must be_==('t1)
                t.dependencies must be_==(List('d1, 'd2, 'd3, 'd4, 'd5, 'd6))
            }
        }
        
        "when the second has an action" in {
            "return the second target with the same name" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5))
                val t2 = Target('t1, List('d2, 'd4, 'd6), {})
                val t  = Target.merge(t1, t2)
                (t eq t1) must be_==(false)
                (t eq t2) must be_==(true)
                t.name must be_==('t1)
            }
            
            """return the second target with the merged dependencies with duplicates removed, 
               with preference to the first targets dependency list""" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5))
                val t2 = Target('t1, List('d2, 'd4, 'd6), {})
                val t  = Target.merge(t1, t2)
                (t eq t1) must be_==(false)
                (t eq t2) must be_==(true)
                t.dependencies must be_==(List('d1, 'd2, 'd3, 'd4, 'd5, 'd6))
            }
        }
        
        "when both have an action" in {
            "throw a BuildError" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5), {})
                val t2 = Target('t1, List('d2, 'd4, 'd6), {})
                Target.merge(t1, t2) must throwA[BuildError]
            }
        }
    }
}