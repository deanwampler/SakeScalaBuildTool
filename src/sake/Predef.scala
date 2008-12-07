package sake

object Predef {
    
    import scala.collection.mutable.Map
    import sake.targets._
    
    /**
     * The default "classpath" is a list of strings containing ".", 
     * the current working directory. Use standard list operators to change it.
     */
    var classpath = List[String](".")
    
    /**
     * Manager of the defined targets and their relationships.
     */
     
    val targetManager = new TargetManager
         
    // TODO
    def build = println(targetManager.allTargets())
    
    /**
     * Create one or more targets, passed in as a vararg list of Strings and/or
     * Symbols or Lists of the same. The last argument is the action for the the
     * target. Use "{}" for "do nothing".
     */
    def target(targets: Any*) = targets.foldLeft(new TargetGroup()) {
        (group, targ) =>
        group ::: (targ match {
            case (n, deps) => Target.makeTarget(n, deps)
            case n         => Target.makeTarget(n, Nil)
        })
    }
}
