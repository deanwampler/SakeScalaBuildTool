package sake.command

import sake.util._

class Command[A,B](val name: String, val defaultOptions: Option[Map[A,B]]) {
    
    def this(name:String, defaultOpts: Map[A,B]) = this(name, Some(defaultOpts))
    def this(name:String, defaultOpt0: (A,B), defaultOpts: (A,B)*) = 
        this(name, Map[A,B](defaultOpt0)++defaultOpts)
    def this(name:String) = this(name, None)
    
    def apply(options: (A,B)*) = {
        val opts = defaultOptions match {
            case Some(map) => map ++ options
            case None => Map[A,B]() ++ options
        }
        postFilterResult(
            action(
                new Passed(), 
                filterOptions(opts)))
    }

    /**
     * Override as needed in subclasses, The user specified options will be checked to confirm
     * that any required options are present.
     */
    val requiredOptions: List[A] = Nil
    
    private def filterOptions(options: Map[A,B]) = {
        val missingOptions = for { 
            key <- requiredOptions
            if (! options.contains(key)) 
        } yield key
        if (missingOptions.length > 0)
            throw new BuildError(name+" requires option(s) "+missingOptions)
        optionsPostFilter(options)
    }

    /**
    * Override as needed in subclasses, _e.g.,_ to handle option "aliases" or
    * to verify the _values_ for certain _keys_ are valid.
     */
    protected def optionsPostFilter(options: Map[A,B]) = options

    /**
     * Override as needed in subclasses. This is the method that does the normal work
     * of the command. The override can discard the passed in result or return it, if 
     * the action does nothing.
     */
    protected def action(result: Result, options: Map[A,B]) = result

    /**
     * Override as needed in subclasses. Perhaps to recover from an error.
     * Users can post-process the result using the "command(...) and { result => ... }"
     * idiom. 
     */
    protected def postFilterResult(result: Result) = result
}
        
