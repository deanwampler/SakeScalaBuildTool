package sake

import org.specs._ 

object PredefSpec extends Specification {
    
    import sake.Predef._
    import sake.targets._
    
    def verifyDeps(t: Target, deps: List[Symbol]) = {
        t.dependencies.size must be_==(deps.size)
        for (i <- 0 to (deps.size - 1)) {
            t.dependencies(i) must be_==(deps(i))
        }
    }
    
    "The target method specification" should {
        "accept a symbol as its name as the first parameter" in {
            val group = target('targetName)
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }

        "accept a string as its name as the first parameter" in {
            val group = target("targetName")
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }

        "convert an input string name to a symbol" in {
            val group = target("targetName")
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }
        
        "accept more than one symbol and/or string as the names of new targets, as the first parameter" in {
            val group = target('targetName1, "targetName2", "targetName3", 'targetName4)
            group.targets.length must be_==(4)
            val t = group.targets.head
            List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a Nil list names and create no new targets" in {
            val group = target(Nil)
            group.targets.length must be_==(0)
        } 

        """accept a list of one or more symbols and/or strings as the names of new targets, 
            as the first parameter""" in {
            val targs = List('targetName1, "targetName2", "targetName3", 'targetName4)
            val group = target(targs)
            group.targets.length must be_==(4)
            List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a single dependent after each name (and convert it to a List)" in {
            val group = target('targetName1 -> 'dep11, "targetName2" -> "dep21")
            group.targets.length must be_==(2)
            Map('targetName1 -> List('dep11), 'targetName2 -> List('dep21)).foreach { n_d => 
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }

        "accept a List of dependents after each name" in {
            val group = target('targetName1 -> List('dep11, "dep12"), "targetName2" -> ("dep21" :: 'dep22 :: Nil))
            group.targets.length must be_==(2)
            Map('targetName1 -> List('dep11, 'dep12), 'targetName2 -> List('dep21, 'dep22)).foreach { n_d => 
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }


        "accept a List of dependents after a list of name, where each target gets the same list of dependencies" in {
            val group = target(List('targetName1, "targetName2") -> List('depa, "depb", 'depc))
            group.targets.length must be_==(2)
            List('targetName1, 'targetName2).foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, List('depa, 'depb, 'depc))
                }
            }
        }
    }
    
    "The target method with an action" should {
        "accept the action as a no-argument closure" in {
            var invoked = 0
            val group = target('targetName) { 
                invoked += 1
            }
            val t = group.targets.head
            t.build()
            t.build()
            invoked must be_==(2)
        }
    }
}