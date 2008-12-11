package sake

import sake.command.builtin.Commands
import scala.collection.immutable._
import sake.environment._
import sake.target._
import sake.util._


class Project extends Commands {
    
    val environment = Environment.environment
    var log = Log.log
    val Level = sake.util.Level
    
    var showStackTraces = true
    
    private var allTargetGrps: List[TargetGroup] = Nil
    
    def allTargetGroups = allTargetGrps.reverse // return in declared order
    
    private var allTargs: Map[Symbol, Target] = Map().empty
    
    def allTargets = {
        if (allTargs.size == 0)
            allTargs ++= rationalizeTargetsFromGroups()
        allTargs        
    }
    
    def build(targs: String):Unit = build(targs.split(" ").map(Symbol(_)))
    
    def build(targs: Collection[Symbol]):Unit = targs.foreach { t =>
        allTargets.get(t) match {
            case None => Exit.error("No target "+t+" found!")
            case Some(targ) => doBuild(targ)
        }
    }
    
    protected def doBuild(t: Target) = {
        t.dependencies match {
            case Nil  =>
            case deps => build(deps)
        }
        log(Level.Info, "building "+t.name)
        try {
            t.build()
        } catch {
            case BuildError(msg, th) => handleBuildError(msg, th)
        }
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
    
    private def handleBuildError(msg: String, th: Throwable) {
        if (showStackTraces && th != null) {
            th.printStackTrace(log.out)
        }
        System.exit(1)
    }
}
