package sake.command.builtin

import sake.environment._
import sake.util._

/**
 * Run all the specs in a path, filtered by a name pattern.
 * The default options are
 * <ol>
 *  <li><em>'path</em> => directory path. Any pattern, inluding "globs", allowed by 
 *  {@link org.specs.runner.SpecsFinder} can be used. Defaults to "spec".</li>
 *  <li><em>'pattern</em> => spec class or object name filter regex. Defaults to ".*".</li>
 * </ol>
 */
class SpecCommand(defaultOptions: Option[Map[Symbol, Any]]) 
    extends Command[Symbol, Any]("spec", defaultOptions){

    def this(defaultOpts: Map[Symbol, Any]) = this(Some(defaultOpts))

    def this(defaultOpt0: (Symbol, Any), defaultOpts: (Symbol, Any)*) = 
        this(Map[Symbol, Any](defaultOpt0)++defaultOpts)

    def this() = this(None)

    private def optionProcessor(key: Symbol, value: Any): Option[List[Any]] = 
        key match {
            case 'path    => Some(List(value.toString()))
            case 'pattern => Some(List(value.toString()))
            case 'report  => Some(List(toBoolean(value)))
            case _  => None
        }

    protected val optionsProcessor = new OptionsProcessor[Symbol, Any]().addProcessor(optionProcessor _)
    
    override def action(options: Map[Symbol, Any]): Result = {
        val path     = options.getOrElse('path,    "spec").toString()
        val pattern  = options.getOrElse('pattern, ".*").toString()
        val doReport = toBoolean(options.getOrElse('report,  true))
        
        if (Environment.environment.dryRun == true)
            return new Passed()
        runSpecs(makeSpecsFileRunner(path, pattern), doReport)
    }
    
    import org.specs._
    import org.specs.runner._
    
    protected def makeSpecsFileRunner(path: String, pattern: String) = 
        new SpecsFileRunner(path, pattern)
        
    protected def runSpecs(runner: SpecsFileRunner, outputReport: Boolean): Result = {
        if (outputReport)
            runner.report(runner.specs)
        runner.specs foreach { spec => 
            if (spec.isFailing)
                return new Failed()
        }
        new Passed()
    }
    
    private def toBoolean(value: Any): Boolean = value match {
        case b:Boolean => b
        case _ => Exit.error("Must specify true or false for the 'report option.")
    }
    
}

