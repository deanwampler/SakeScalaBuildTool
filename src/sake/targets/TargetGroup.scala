package sake.targets

class TargetGroup(val targets: List[Target]) {
    
    def this(t: Target) = this(List(t))
    def this() = this(List())
    
    def +(t: Target) = new TargetGroup(t :: targets)
    
    def ++(tg: TargetGroup) = new TargetGroup(tg.targets ::: targets)
    
    /**
     * Create new Targets, _replacing_ the existing action (if any) with the new action.
     */
    def action(act: => Unit) = new TargetGroup(for {
        t <- targets
    }   yield new Target(t.name, t.dependencies, act))
}