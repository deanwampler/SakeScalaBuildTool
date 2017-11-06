package sake

import org.specs._ 

object TargetSpec extends Specification {
    
    import sake.target._
    import sake.util._
    import sake.command._
    
    def checkDependencies(t: Target, expected: List[Symbol]) =
            t.dependencies mustEqual expected


    "A new Target created with just a name" should {
        "have the same name" in {
            (Target('t)).name mustEqual 't
        }

        "have no dependencies" in {
            checkDependencies(Target('t), Nil)
        }
    }
    
    "A new Target created with a name and a list of dependencies" should {
        "have the same name" in {
            (Target('t, Nil)).name mustEqual 't
        }

        "have the same dependency list" in {
            checkDependencies(Target('t, Nil), Nil)
            checkDependencies(Target('t, List('d1)), List('d1))
            checkDependencies(Target('t, List('d1, 'd2)), List('d1, 'd2))
        }
    }
    
    "A new Target created with a name, a list of dependencies, and an action" should {
        "have the same name" in {
            (Target('t, Nil, {})).name mustEqual 't
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
            success mustEqual true
        }
    }
    
    "Calling build() on a Target" should {
        "invoke the Target's build action" in {
            var success = false
            val t = Target('t, List('d1), { success = true })
            t.build()
            success mustEqual true
        }
    }
    
    "Calling build() on a Target that fails" should {
        "throw a BuildError" in {
            val t = Target('t, List('d1), { 
                val c = new Command[Symbol,String]("failure") {
                    override def action(o: Map[Symbol,String]) = new Failed()
                }
                c()
             })
            t.build() must throwA[BuildError]
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
                    t.name mustEqual 't1
                    t.dependencies.size mustEqual 0
                }
            }

            "when the first has dependencies" in {
                "return a target with the dependencies from the first" in {
                    val t = Target.merge(Target('t1, List('d1, 'd2)), Target('t1))
                    t.name mustEqual 't1
                    t.dependencies mustEqual List('d1, 'd2)
                }
            }

            "when the second has dependencies" in {
                "return a target with the dependencies from the second" in {
                    val t = Target.merge(Target('t1), Target('t1, List('d1, 'd2)))
                    t.name mustEqual 't1
                    t.dependencies mustEqual List('d1, 'd2)
                }
            }

            "when both have dependencies" in {
                "return a target with the merged dependencies with duplicates removed" in {
                    val t = Target.merge(Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5)), 
                                         Target('t1, List('d2, 'd4, 'd6)))
                    t.name mustEqual 't1
                    t.dependencies mustEqual List('d1, 'd2, 'd3, 'd4, 'd5, 'd6)
                }
            }
        }
        
        "when the first has an action" in {
            "return the first target with the same name" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5), {})
                val t2 = Target('t1, List('d2, 'd4, 'd6))
                t1.isInstanceOf[TargetWithAction] mustEqual true
                val t  = Target.merge(t1, t2)
                (t eq t1) mustEqual true
                (t eq t2) mustEqual false
                t.name mustEqual 't1
            }
            
            """return the first target with the merged dependencies with duplicates removed, 
               with preference to the first targets dependency list""" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5), {})
                val t2 = Target('t1, List('d2, 'd4, 'd6))
                val t  = Target.merge(t1, t2)
                (t eq t1) mustEqual true
                (t eq t2) mustEqual false
                t.name mustEqual 't1
                t.dependencies mustEqual List('d1, 'd2, 'd3, 'd4, 'd5, 'd6)
            }
        }
        
        "when the second has an action" in {
            "return the second target with the same name" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5))
                val t2 = Target('t1, List('d2, 'd4, 'd6), {})
                val t  = Target.merge(t1, t2)
                (t eq t1) mustEqual false
                (t eq t2) mustEqual true
                t.name mustEqual 't1
            }
            
            """return the second target with the merged dependencies with duplicates removed, 
               with preference to the first targets dependency list""" in {
                val t1 = Target('t1, List('d1, 'd2, 'd3, 'd4, 'd5))
                val t2 = Target('t1, List('d2, 'd4, 'd6), {})
                val t  = Target.merge(t1, t2)
                (t eq t1) mustEqual false
                (t eq t2) mustEqual true
                t.dependencies mustEqual List('d1, 'd2, 'd3, 'd4, 'd5, 'd6)
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