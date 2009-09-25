package sake.command.builtin

import sake.command.ShellCommand
import sake.environment.Environment._
import sake.util._
import sake.util.Path._

class JVMCommand(name: String, options: Option[Map[Symbol,Any]]) extends ShellCommand(name, options) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name:String) = this(name, None)

    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'class     => Some(List(stringize(value)))
            case 'cp        => Some("-classpath" :: pathToString(value) :: Nil)
            case 'classpath => Some("-classpath" :: pathToString(value) :: Nil)
            case _          => None
        }
    
    optionsProcessor.addProcessor(optionProcessor _)
    
    // Make sure the classpath is set. I'm not sure why, but the environment.getSystemEnv.get("CLASSPATH") actually has
    // the classpath we want!
    override protected def optionsPostFilter(options: Map[Symbol,Any]) = {
        val (key, cp) = if (options.contains('cp))
            ('cp, options.getOrElse('cp, NilPath))
        else if (options.contains('classpath))
            ('classpath, options.getOrElse('classpath, NilPath))
        else 
            ('cp, NilPath)
        val cpath = cp match {
            case p: Path => p
            case s: Seq[_] => Path(s)
            case _ => Path(cp.toString)
        }
        super.optionsPostFilter(options.update(key, cpath ::: environment.classpath))
    }
}

