package sake

object Predef {
    
    import sake.targets._
    
    /**
     * The default "classpath" is a list of strings containing ".", 
     * the current working directory. Use standard list operations to change it.
     */
    var classpath = List[String](".")
    
    /**
     * Manager of the defined targets and their relationships. 
     * TODO
     */
     
    val targetManager = new TargetManager
         
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
