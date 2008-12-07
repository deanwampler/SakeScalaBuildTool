package sake {
    import org.specs._ 
    import sake.Predef._

    object PredefSpec extends Specification {
        
        def verifyTarget(targetName: Symbol) = {
            val target = getTarget(targetName)
            target.name must be_==(targetName)
        }

        def verifyDeps(targetName: Symbol, deps: List[Symbol]) = {
            val target = getTarget(targetName)
            target.dependencies.size must be_==(deps.size)
            for (i <- 0 to (deps.size - 1)) {
                target.dependencies(i) must be_==(deps(i))
            }
        }
        
        def getTarget(targetName: Symbol) = allTargets.get(targetName) match {
            case None => fail(targetName.toString())
            case Some(t) =>  t
        }
        
        "The target method with no action function" should {
            doBefore {
                allTargets.clear()
                allTargets.size must be_==(0)
            }
            "accept a symbol as its name as the first parameter" in {
                target('targetName) {}
                allTargets.size must be_==(1)
                verifyTarget('targetName)
                verifyDeps('targetName, Nil)
            }

            "accept a string as its name as the first parameter" in {
                target("targetName") {}
                allTargets.size must be_==(1)
                verifyTarget('targetName)
                verifyDeps('targetName, Nil)
            }

            "convert an input string name to a symbol" in {
                target("targetName") {}
                verifyTarget('targetName)
                verifyDeps('targetName, Nil)
            }
            
            "accept more than one symbol and/or string as the names of new targets, as the first parameter" in {
                target('targetName1, "targetName2", "targetName3", 'targetName4) {}
                allTargets.size must be_==(4)
                List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                    verifyTarget(n)
                    verifyDeps(n, Nil)
                }
            }

            "accept a Nil list names and create no new targets" in {
                target(Nil) {}
                allTargets.size must be_==(0)
            } 

            """accept a list of one or more symbols and/or strings as the names of new targets, 
                as the first parameter""" in {
                val targs = List('targetName1, "targetName2", "targetName3", 'targetName4)
                target(targs) {}
                allTargets.size must be_==(4)
                List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                    verifyTarget(n)
                    verifyDeps(n, Nil)
                }
            }

            "accept a single dependent after each name (and convert it to a List)" in {
                target('targetName1 -> 'dep11, "targetName2" -> "dep21") {}
                allTargets.size must be_==(2)
                List('targetName1, 'targetName2).foreach {verifyTarget(_)}
                verifyDeps('targetName1, List('dep11))
                verifyDeps('targetName2, List('dep21))
            }

            "accept a List of dependents after each name" in {
                target('targetName1 -> List('dep11, "dep12"), "targetName2" -> ("dep21" :: 'dep22 :: Nil)) {}
                allTargets.size must be_==(2)
                List('targetName1, 'targetName2).foreach {verifyTarget(_)}
                verifyDeps('targetName1, List('dep11, 'dep12))
                verifyDeps('targetName2, List('dep21, 'dep22))
            }


            "accept a List of dependents after a list of name, where each target gets the same list of dependencies" in {
                target(List('targetName1, "targetName2") -> List('depa, "depb", 'depc)) {}
                allTargets.size must be_==(2)
                List('targetName1, 'targetName2).foreach { n =>
                    verifyTarget(n)
                    verifyDeps(n, List('depa, 'depb, 'depc))
                }
            }
        }
        
        "The target method with an action function" should {
            doBefore {
                allTargets.clear()
                allTargets.size must be_==(0)
            }
            "accept the action as a no-argument closure" in {
                var invoked = 0
                target('targetName) { 
                    invoked += 1
                }
                val t = getTarget('targetName)
                t.invoke()
                t.invoke()
                invoked must be_==(2)
            }
        }
        
    }
}