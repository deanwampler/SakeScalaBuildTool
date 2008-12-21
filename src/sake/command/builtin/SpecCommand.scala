package sake.command.builtin

/**
 * Run all the specs in a path, filtered by a name pattern.
 * The default options are
 * <ol>
 *  <li><em>'path</em> => directory path. Any pattern, inluding "globs", allowed by 
 *  {@link org.specs.runner.SpecsFinder} can be used. Defaults to "spec".</li>
 *  <li><em>'pattern</em> => spec class or object name filter regex. Defaults to ".*".</li>
 * </ol>
 */
class SpecCommand(defaultOptions: Option[Map[Symbol, String]]) 
    extends Command[Symbol, String]("spec", defaultOptions){

    def this(defaultOpts: Map[Symbol,String]) = this(Some(defaultOpts))

    def this(defaultOpt0: (Symbol,String), defaultOpts: (Symbol,String)*) = 
        this(Map[Symbol,String](defaultOpt0)++defaultOpts)

    def this() = this(None)

    private def optionProcessor(key: Symbol, value: String): Option[List[String]] = 
        key match {
            case 'path    => Some(List(value.toString()))
            case 'pattern => Some(List(value.toString()))
            case _  => None
        }

    protected val optionsProcessor = new OptionsProcessor[Symbol,String]().addProcessor(optionProcessor _)
    
    override def action(options: Map[Symbol, String]):Result = {
        val path    = options.getOrElse('path,    "spec")
        val pattern = options.getOrElse('pattern, ".*")
        
        makeSpecsFileRunner(path, pattern).specs.foreach { spec => 
            runSpec(spec)
        }
        new Passed()
    }
    
    protected def makeSpecsFileRunner(path: String, pattern: String) = 
        new org.specs.runner.SpecsFileRunner(path, pattern)
        
    protected def runSpec(spec: org.specs.Specification) = spec.main(Array[String]())
}

