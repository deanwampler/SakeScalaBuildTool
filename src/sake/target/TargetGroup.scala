package sake.target

import sake.util._

/**
 * Wraps a list of targets and provides an apply() method to specify the 
 * "action" function for all the targets.
 */ 
class TargetGroup(val targets: List[Target]) {
    
    def this(t: Target) = this(List(t))
    def this() = this(Nil)
    
    def ::(t: Target) = new TargetGroup(t :: targets)
    
    def :::(tg: TargetGroup) = new TargetGroup(tg.targets ::: targets)
    
    /**
     * An action is assigned to the Targets in the TargetGroup.
     * Creates new Targets, _replacing_ the existing action in each one (if any) with the new action.
     */
    def apply(act: => Unit) = {
        val targetList = for {
            t <- targets
        }   yield Target(t.name, t.dependencies, act)
        new TargetGroup(targetList)
    }
    
    override def equals(other: Any) = other match {
        case tg: TargetGroup => targets.equals(tg.targets)
        case _ => false
    }
}

object TargetGroup {
    
    def apply(names: Any, deps: Any): TargetGroup = names match {
        case head :: tail => TargetGroup(head, deps) ::: TargetGroup(tail, deps)
        case Nil => new TargetGroup()
        case _ => {
            val name = SymbolUtil.toSymbol(names)
            new TargetGroup(Target(name, toList(deps)))
        }
    }
    
    private def toList(item: Any) = item match { 
        case iter: Collection[_] => SymbolUtil.toSymbols(iter)
        case _ => List[Symbol](SymbolUtil.toSymbol(item))
    }
}
