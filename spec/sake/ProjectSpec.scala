package sake

import org.specs._ 

object ProjectSpec extends Specification {
    
    import sake.target._
    import sake.util._
    
    def verifyDeps(t: Target, deps: List[Symbol]) = {
        t.dependencies.size must be_==(deps.size)
        for (i <- 0 to (deps.size - 1)) {
            t.dependencies.contains(deps(i)) must be_==(true)
        }
    }
    
    "The target method" should {
        "accept a symbol as its name as the first parameter" in {
            val group = new Project().target('targetName)
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }

        "accept a string as its name as the first parameter" in {
            val group = new Project().target("targetName")
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }

        "convert an input string name to a symbol" in {
            val group = new Project().target("targetName")
            group.targets.length must be_==(1)
            val t = group.targets.head
            t.name must be_==('targetName)
            verifyDeps(t, Nil)
        }
        
        "accept more than one symbol and/or string as the names of new targets, as the first parameter" in {
            val group = new Project().target('targetName1, "targetName2", "targetName3", 'targetName4)
            group.targets.length must be_==(4)
            val t = group.targets.head
            List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a Nil list of names and create no new targets" in {
            val group = new Project().target(Nil)
            group.targets.length must be_==(0)
        } 

        """accept a list of one or more symbols and/or strings as the names of new targets, 
            as the first parameter""" in {
            val targs = List('targetName1, "targetName2", "targetName3", 'targetName4)
            val group = new Project().target(targs)
            group.targets.length must be_==(4)
            List('targetName1, 'targetName2, 'targetName3, 'targetName4).foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a single dependent after each name (and convert it to a List)" in {
            val group = new Project().target('targetName1 -> 'dep11, "targetName2" -> "dep21")
            group.targets.length must be_==(2)
            Map('targetName1 -> List('dep11), 'targetName2 -> List('dep21)).foreach { n_d => 
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }

        "accept a List of dependents after each name" in {
            val group = new Project().target('targetName1 -> List('dep11, "dep12"), "targetName2" -> ("dep21" :: 'dep22 :: Nil))
            group.targets.length must be_==(2)
            Map('targetName1 -> List('dep11, 'dep12), 'targetName2 -> List('dep21, 'dep22)).foreach { n_d => 
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }


        "accept a List of dependents after a list of name, where each target gets the same list of dependencies" in {
            val group = new Project().target(List('targetName1, "targetName2") -> List('depa, "depb", 'depc))
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
        "accept the action as a no-argument closure returning Unit" in {
            var invoked = 0
            val group = new Project().target('targetName) { 
                invoked += 1
            }
            val t = group.targets.head
            t.build()
            t.build()
            invoked must be_==(2)
        }

        "include the target name in the exception message if the build fails" in {
            var invoked = 0
            val group = new Project().target('FailedTarget) { 
                throw new BuildError("test")
            }
            val t = group.targets.head
            try {
                t.build()
            } catch {
                case BuildError(m, th) => m.contains("FailedTarget") must be_==(true)
                case _ => fail()
            }
        }
    }
    
    "allTargetGroups" should {
        "return all the groups that have been created in a project" in {
            val project = new Project()
            val group1 = project.target('t1, 't2)
            val group2 = project.target('t2, 't3, 't4)
            project.allTargetGroups match {
                case group2 :: l => l match {
                    case group1 :: Nil =>
                    case _ => fail("second element is not group1")
                }
                case _ => fail("didn't match group2 :: list")
            }
        }
    } 

    "allTargets" should {
        "return all the targets that have been created in a project" in {
            val project = new Project()
            project.target('t1, 't2)
            project.target('t2, 't3, 't4)
            project.allTargets.size must be_==(4) 
            List('t1, 't2, 't3, 't4).foreach { t =>
                project.allTargets.contains(t) must be_==(true)
            }
        }

        "merge all the duplicate targets" in {
            val project = new Project()
            project.target(List('t1, 't2, 't3) -> List('d1, 'd2, 'd3))
            project.target(List('t2, 't4) -> List('d2, 'd4)) {}
            project.allTargets.size must be_==(4) 

            def checkTarget(project:Project, name:Symbol, expectedDeps:List[Symbol]) = {
                project.allTargets.get(name) match {
                    case None => fail(name.toString())
                    case Some(t) => {
                        t.name must be_==(name)
                        t.dependencies must be_==(expectedDeps)
                    }
                }
            }
            checkTarget(project, 't1, List('d1, 'd2, 'd3))
            checkTarget(project, 't2, List('d1, 'd2, 'd3, 'd4))
            checkTarget(project, 't3, List('d1, 'd2, 'd3))
            checkTarget(project, 't4, List('d2, 'd4))
        }
    } 
}