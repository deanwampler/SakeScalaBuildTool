package sake.command

import sake.util._

case class Result[R](success: Boolean, result: Option[R], message: String)

class Command[R](val name: String, val defaultOptions: Map[Any, Any]) {
    def apply(options: Map[Any, Any]) = {
        postFilterResult(
            action(
                new Result[R](true, None, ""), 
                filterOptions(defaultOptions ++ options)))
    }
    
    /**
     * Override as needed in subclasses, The user specified options will be checked to confirm
     * that any required options are present.
     */
    val requiredOptions: List[String] = Nil
    
    /**
     * Override as needed in subclasses, _e.g.,_ to handle option "aliases" or
     * to very required options are present.
     */
    def filterOptions(options: Map[Any, Any]) = {
        val missingOptions = for { 
            key <- requiredOptions
            if (! options.contains(key)) 
        } yield key
        if (missingOptions.length > 0)
            throw new BuildError(name+" requires option(s) "+missingOptions)
        postFilterOptions(options)
    }

    /**
    * Override as needed in subclasses, _e.g.,_ to handle option "aliases" or
    * to verify the _values_ for certain _keys_ are valid.
     */
    def postFilterOptions(options: Map[Any, Any]) = options

    /**
     * Override as needed in subclasses. This is the method that does the normal work
     * of the command. The override can discard the passed in result or return it, if 
     * the action was successful.
     */
    def action[R](successfulResult: Result[R], options: Map[Any, Any]) = successfulResult

    /**
     * Override as needed in subclasses. Perhaps to recover from an error.
     */
    def postFilterResult[R](result: Result[R]) = result
}