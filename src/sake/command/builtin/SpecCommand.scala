package sake.command.builtin

import sake.environment._
import sake.util._

/**
 * Run all the specs in a path, filtered by a name pattern.
 * The default options are
 * <ol>
 *  <li><em>'path</em> => directory path. Any pattern, inluding "globs", allowed by 
 *  {@link org.specs.runner.SpecsFinder} can be used. Defaults to "spec" (a presumed directory for "spec" source files).</li>
 *  <li><em>'pattern</em> => spec class or object name filter regex. Defaults to ".*".</li>
 * </ol>
 * @note Must run as a separate process to pick up any newly compiled files, rather than reading what's in the sake.jar file!
 */
class SpecCommand(defaultOptions: Option[Map[Symbol, Any]]) extends JVMCommand("scala", defaultOptions){

    def this(defaultOpts: Map[Symbol, Any]) = this(Some(defaultOpts))

    def this(defaultOpt0: (Symbol, Any), defaultOpts: (Symbol, Any)*) = 
        this(Map[Symbol, Any](defaultOpt0)++defaultOpts)

    def this() = this(None)

    private def optionProcessor(key: Symbol, value: Any): Option[List[Any]] = 
        key match {
            case 'path    => Some(List(value.toString()))
            case 'pattern => Some(List(value.toString()))
            case _  => None
        }

    optionsProcessor.addProcessor(optionProcessor _)
    
    override protected def optionsPostFilter(options: Map[Symbol,Any]) = {
        val path    = options.getOrElse('path,    SakeSpecRunner.defaultPath).toString()
        val pattern = options.getOrElse('pattern, SakeSpecRunner.defaultPattern).toString()
        val opts    = options.getOrElse('opts, Nil) match {
            case l: List[_] => l
            case o => List(o)
        }
//        val scalaScript = "import org.specs.runner._; val r = new SpecsFileRunner(\"" +
//                          path + "\", \"" + pattern + "\"); r.report(r.specs)"
//        super.optionsPostFilter(removePathAndPattern(options.update('opts, "-e" :: scalaScript :: opts)))
        val scalaScriptOpts = "sake.command.builtin.SakeSpecRunner" :: path :: pattern :: Nil
        super.optionsPostFilter(removePathAndPattern(options.update('opts, opts ::: scalaScriptOpts)))
    }
    
    protected def removePathAndPattern(options: Map[Symbol,Any]) = options - 'path - 'pattern
}

import org.specs.runner._

/** 
 * Simple command line tool to run the specs in the given file/directory specification (1st argument)
 * matching the given pattern (2nd argument).
 * When running this object separately, use the command,
 *   scala -classpath lib/specs-1.4.3.jar:build sake.command.builtin.SakeSpecRunner [path [pattern]]
 */
object SakeSpecRunner {
    val defaultPath    = "spec"
    val defaultPattern = ".*Spec.*"
    
    def main(args: Array[String]) = {
        val path    = if (args.length >= 1) args(0) else defaultPath
        val pattern = if (args.length >= 2) args(1) else defaultPattern
        runSpecs(path, pattern)
    }
    
    def runSpecs(path: String, pattern: String) = {
        val runner = new SpecsFileRunner(path, pattern)
        runner.report(runner.specs)
        if (runner.specs.exists(_.isFailing)) System.exit(1) else System.exit(0)  
    }
}