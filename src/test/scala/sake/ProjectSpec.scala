package sake

import org.scalatest._
import org.scalatest.Matchers._

object ProjectSpec extends FreeSpec {

    import sake.target._
    import sake.util._

    def verifyDeps(t: Target, deps: List[Symbol]) = {
        assert(t.dependencies.size shouldEqual deps.size
        for (i <- 0 to (deps.size - 1)) {
            t.dependencies.contains(deps(i)) shouldEqual true
        }
    }

    "The target method" should {
        "accept a symbol as its name as the first parameter" in {
            val group = Project().target('targetName)
            group.targets.length shouldEqual 1
            val t = group.targets.head
            t.name shouldEqual 'targetName
            verifyDeps(t, Nil)
        }

        "accept a string as its name as the first parameter" in {
            val group = Project().target("targetName")
            group.targets.length shouldEqual 1
            val t = group.targets.head
            t.name shouldEqual 'targetName
            verifyDeps(t, Nil)
        }

        "convert an input string name to a symbol" in {
            val group = Project().target("targetName")
            group.targets.length shouldEqual 1
            val t = group.targets.head
            t.name shouldEqual 'targetName
            verifyDeps(t, Nil)
        }

        "accept more than one symbol and/or string as the names of new targets, as the first parameter" in {
            val group = Project().target('targetName1, "targetName2", "targetName3", 'targetName4)
            group.targets.length shouldEqual 4
            val t = group.targets.head
            List('targetName1, 'targetName2, 'targetName3, 'targetName4) foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a Nil list of names and create no new targets" in {
            val group = Project().target(Nil)
            group.targets.length shouldEqual 0
        }

        """accept a list of one or more symbols and/or strings as the names of new targets,
            as the first parameter""" in {
            val targs = List('targetName1, "targetName2", "targetName3", 'targetName4)
            val group = Project().target(targs)
            group.targets.length shouldEqual 4
            List('targetName1, 'targetName2, 'targetName3, 'targetName4) foreach { n =>
                group.targets.find(t => t.name == n) match {
                    case None => fail(n.toString())
                    case Some(t) => verifyDeps(t, Nil)
                }
            }
        }

        "accept a single dependent after each name (and convert it to a List)" in {
            val group = Project().target('targetName1 -> 'dep11, "targetName2" -> "dep21")
            group.targets.length shouldEqual 2
            Map('targetName1 -> List('dep11), 'targetName2 -> List('dep21)) foreach { n_d =>
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }

        "accept a List of dependents after each name" in {
            val group = Project().target('targetName1 -> List('dep11, "dep12"), "targetName2" -> ("dep21" :: 'dep22 :: Nil))
            group.targets.length shouldEqual 2
            Map('targetName1 -> List('dep11, 'dep12), 'targetName2 -> List('dep21, 'dep22)) foreach { n_d =>
                group.targets.find(t => t.name == n_d._1) match {
                    case None => fail(n_d._1.toString())
                    case Some(t) => verifyDeps(t, n_d._2)
                }
            }
        }


        "accept a List of dependents after a list of name, where each target gets the same list of dependencies" in {
            val group = Project().target(List('targetName1, "targetName2") -> List('depa, "depb", 'depc))
            group.targets.length shouldEqual 2
            List('targetName1, 'targetName2) foreach { n =>
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
            val group = Project().target('targetName) {
                invoked += 1
            }
            val t = group.targets.head
            t.build()
            t.build()
            invoked shouldEqual 2
        }

        "include the target name in the exception message if the build fails" in {
            var invoked = 0
            val group = Project().target('FailedTarget) {
                throw new BuildError("failure text")
            }
            val t = group.targets.head
            try {
                t.build()
            } catch {
                case BuildError(m, th) => m.contains("FailedTarget") shouldEqual true
                case _ => fail()
            }
        }
    }

    "allTargetGroups" should {
        "return all the groups that have been created in a project" in {
            val project = Project()
            val group1 = project.target('t1, 't2)
            val group2 = project.target('t2, 't3, 't4)
            project.allTargetGroups must containAll(group1 :: group2 :: Nil)
        }
    }

    "allTargets" should {
        "return all the targets that have been created in a project" in {
            val project = Project()
            project.target('t1, 't2)
            project.target('t2, 't3, 't4)
            project.allTargets.size shouldEqual 4
            List('t1, 't2, 't3, 't4) foreach { t =>
                project.allTargets.contains(t) shouldEqual true
            }
        }

        "merge all the duplicate targets" in {
            val project = Project()
            project.target(List('t1, 't2, 't3) -> List('d1, 'd2, 'd3))
            project.target(List('t2, 't4) -> List('d2, 'd4)) {}
            project.allTargets.size shouldEqual 4

            def checkTarget(project:Project, name:Symbol, expectedDeps:List[Symbol]) = {
                val t = project.allTargets.get(name).get
                t.name shouldEqual name
                t.dependencies shouldEqual expectedDeps
            }
            checkTarget(project, 't1, List('d1, 'd2, 'd3))
            checkTarget(project, 't2, List('d1, 'd2, 'd3, 'd4))
            checkTarget(project, 't3, List('d1, 'd2, 'd3))
            checkTarget(project, 't4, List('d2, 'd4))
        }
    }

    "building multiple targets with shared dependencies" should {
        "will eliminate duplicates and build in the correct order" in {
            val project = Project() {
                override def determineTargets(targs: List[Symbol]):List[Target] = {
                    val list = super.determineTargets(targs)
                    list.map(_.name) shouldEqual List('t3, 't4, 't2, 't1, 't5)
                    list
                }
            }
            project.target('t1 -> List('t2, 't3))
            project.target('t2 -> List('t3, 't4))
            project.target('t5 -> List('t2))
            project.target(List('t3, 't4))
            project.build("t1 t5")
        }
    }
}
