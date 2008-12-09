package sake

import sake.command.builtin.Commands

trait Project extends Commands {
    
    import sake.environment._
    import sake.targets._

    var classpath = Environment.classpath

    // TODO
    def build(targ: Symbol) = println("building: "+targ.toString())
    
    /**
     * Create one or more targets, passed in as a vararg list of Strings and/or
     * Symbols or Lists of the same.
     * @return TargetGroup containing the new Targets.
     */
    def target(targets: Any*) = targets.foldLeft(new TargetGroup()) {
        (group, targ) =>
        group ::: (targ match {
            case (n, deps) => Target.makeTarget(n, deps)
            case n         => Target.makeTarget(n, Nil)
        })
    }
}
