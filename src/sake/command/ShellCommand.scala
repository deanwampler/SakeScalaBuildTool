package sake.command

import java.lang._
import sake.util._
import sake.util.Path._
import sake.environment._

/**
 * Defining and running arbitrary shell commands. 
 */
class ShellCommand(name: String, defaultOptions: Option[Map[Symbol,Any]]) 
    extends Command[Symbol,Any](name, defaultOptions) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name:String) = this(name, None)

    protected val optionsProcessor = new OptionsProcessor[Symbol,Any]().addProcessor(optionProcessor _)
    
    override def action(options: Map[Symbol,Any]) = {
        val command = determineCommandName(options)
        val commandArgs = buildCommandArgsList(options)
        val commandEnv = buildCommandEnvMap(options)
        Log.log(Level.Notice, "shell: "+toCommandString(command, commandArgs, commandEnv))
        val commandRunner = makeCommandRunner(command, commandArgs, commandEnv)
        if (Environment.environment.dryRun == true)
            new Passed()
        else
            commandRunner.run()
    }
    
    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'files => value match {
                case f:FilesFinder => Some(f())
                case l:List[_] => Some(makeFilesLister()(l.map(_.toString())))
                case x => Some(makeFilesLister()(List(x.toString())))
            }
            case 'command => None    // ignore; handled by determineCommandName
            case 'directory => None  // ignore; handled by buildCommandEnvMap
            case 'D => None          // ignore; handled by buildCommandEnvMap
            case 'inputText => None  // ignore; handled by buildCommandEnvMap
            case 'inputFile => None  // ignore; handled by buildCommandEnvMap
            case 'outputFile => None // ignore; handled by buildCommandEnvMap
            case 'opts => Some(toStringList(value))
            case other => Some(List("-"+stringize(other), pathToString(value)))
        }

   protected def determineCommandName(options: Map[Symbol, Any]) = 
        options.getOrElse('command, name).toString()

    protected def buildCommandArgsList(options: Map[Symbol, Any]) = 
        optionsProcessor.processOptionsToList(options).map(_.toString()).filter(_.length > 0)

    protected def buildCommandEnvMap(options: Map[Symbol, Any]) = {
        var envMap = Map[Any, Any]()
        options.foreach { key_value => 
            key_value._1 match {
                case 'directory => envMap += (key_value._1 -> key_value._2)
                case 'D => envMap += envVar(key_value._2.toString())
                case 'inputText  => envMap += (key_value._1 -> key_value._2)
                case 'inputFile  => envMap += (key_value._1 -> key_value._2)
                case 'outputFile => envMap += (key_value._1 -> key_value._2)
                case _ => // ignore
            }
        }
        if (envMap.isEmpty) None else Some(envMap)
    }

    protected def makeCommandRunner(command: String, args: List[String], env: Option[Map[Any,Any]]) = {
        new CommandRunner(command, args, env)
    }

    // hook for test overrides.
    protected def makeFilesLister() = new FilesFinder()

    protected def pathToString(value: Any) = value match {
        case seq:Seq[_] => new Path(seq).toString()
        case _ => value.toString()
    }
    
    protected def toStringList(value: Any): List[String] = value match {
        case s:String => s.contains(" ") match {
            case true  => toStringListFromList(s.split(" ").toList)
            case false => List(s)
        }
        case l:List[_] => toStringListFromList(l)
        case x => List(stringize(x))
    }
    
    protected def toStringListFromList(list: List[Any]): List[String] = list match {
        case head :: tail => stringize(head) :: toStringListFromList(tail)
        case Nil => Nil
    }
    
    protected def stringize(item: Any) = item match {
        case s:String => s
        case s:Symbol => s.name // not toString()
        case _  => item.toString()
    }

    protected def toCommandString(command: String, list: List[String], env: Option[Map[Any, Any]]) = {
        val cstr = list.foldLeft(command)(_ + " " + _ ) 
        env match {
            case None => cstr
            case Some(e) => cstr + " (environment: " + e.toString() + ")"
        }
    }
        
    protected def envVar(str: String): (String,String) = {
        val keyEqValue = """([^=]+)(=(.*))?""".r
        try {
            val keyEqValue(key, ignore, value) = str
            val v = if (value == null) "" else value
            (key, v)
        } catch {
            case me:MatchError => Exit.error("invalid -D key=value expression given. (The \"=value\" part is optional.) Was: \"-D "+str+"\".")
        }
    }
}

object ShellCommand {
    /**
     * Shell command specified as a string. Note that the string will be split on
     * whitespace into tokens. Use {@link Command.apply(options: (A,B)*)} if some of 
     * the "words" contain whitespace.
     */
    def apply[Symbol,Any](str: String): Result = {
        val list = tokenizeCommandString(str)
        new ShellCommand(list.head).apply('command -> list.head, 'opts -> list.tail)
    }

    def tokenizeCommandString(str: String) =
         str.replaceAll("[\\n\\r]+", " ").split("\\s+").toList.filter(_.length > 0)
}
