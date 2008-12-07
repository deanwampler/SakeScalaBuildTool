package sake {
    
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
            group ++ (targ match {
                case (n, deps) => makeTarget(n, deps)
                case n         => makeTarget(n, Nil)
            })
        }
        
        private def makeTarget(names: Any, deps: Any): TargetGroup = names match {
            case head :: tail => makeTarget(head, deps) ++ makeTarget(tail, deps)
            case Nil => new TargetGroup()
            case _ => {
                val name = toSymbol(names)
                new TargetGroup(new Target(name, toSymbolsList(deps)))
            }
        }
        
        private def toSymbolsList(deps: Any) = deps match { 
            case list: List[_] => toSymbols(list)
            case dep => List(toSymbol(dep))
        }
        
        private def toSymbols(list: List[Any]) = {
            for (item <- list) 
                yield toSymbol(item)
        }

        private def toSymbol(item: Any) = item match {
            case s: Symbol => s
            case s: String => Symbol(s)
            case _         => Exit.error("Unrecognized type of object: \""+item+"\". Expected a Symbol or String.")
        }
    }
}