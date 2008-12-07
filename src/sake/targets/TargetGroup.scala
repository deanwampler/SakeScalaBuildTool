package sake.targets

class TargetGroup(val targets: List[Target]) {
    
    def this(t: Target) = this(List(t))
    def this() = this(Nil)
    
    def ::(t: Target) = new TargetGroup(t :: targets)
    
    def :::(tg: TargetGroup) = new TargetGroup(tg.targets ::: targets)
    
    /**
     * An action is assigned to the Targets in the TargetGroup.
     * Creates new Targets, _replacing_ the existing action in each one (if any) with the new action.
     */
    def apply(act: => Unit) = new TargetGroup(for {
        t <- targets
    }   yield new Target(t.name, t.dependencies, act))
    
    override def equals(other: Any) = other match {
        case tg: TargetGroup => targets.equals(tg.targets)
        case _ => false
    }
}