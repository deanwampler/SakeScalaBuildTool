package sake

import sake.command.builtin.Commands
import scala.collection.immutable._
import sake.environment._
import sake.target._


class Project extends Commands {
    
    var classpath = Environment.classpath

    private var allTargetGrps: List[TargetGroup] = Nil
    
    def allTargetGroups = allTargetGrps
    
    private var allTargs: Map[Symbol, Target] = Map().empty
    
    def allTargets = {
        if (allTargs.size == 0)
            allTargs ++= rationalizeTargetsFromGroups()
        allTargs        
    }
    
    def build(targs: String):Unit = build(targs.split(" ").map(Symbol(_)))
    
    def build(targs: Collection[Symbol]):Unit = targs.foreach { t =>
        println("building: "+t.toString())
    }
    
    /**
     * Create one or more targets, passed in as a vararg list of Strings and/or
     * Symbols or Lists of the same.
     * @return TargetGroup containing the new Targets.
     */
    def target(targets: Any*) = {
        val group = targets.foldLeft(new TargetGroup()) {
            (group, targ) =>
            group ::: (targ match {
                case (n, deps) => TargetGroup(n, deps)
                case n         => TargetGroup(n, Nil)
            })
        }
        allTargetGrps ::= group
        group
    }
    
    private def rationalizeTargetsFromGroups() = 
        allTargetsFromGroups().foldLeft(allTargs) { (map, targ) =>
            val targ2 = map.get(targ.name) match {
                case None => targ
                case Some(t) => Target.merge(t, targ)
            }
            map.update(targ2.name, targ2)
        }
        
    private def allTargetsFromGroups() = {
        for {
            group <- allTargetGroups
            targ  <- group.targets
        } yield targ
    }
    
}
