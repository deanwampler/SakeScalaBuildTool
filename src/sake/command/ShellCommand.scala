package sake.command

import java.lang._
import sake.util._
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
        val commandOpts = buildCommandOptionsList(options)
        Log.log(Level.Notice, "shell: "+toCommandString(command, commandOpts))
        if (Environment.environment.dryRun == true)
            new Passed()
        else
            new CommandRunner(command, commandOpts).run()
    }
    
    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'files => value match {
                case f:FilesFinder => Some(f())
                case l:List[_] => Some(makeFilesLister()(l.map(_.toString())))
                case x => Some(makeFilesLister()(List(x.toString())))
            }
            case 'command => None  // ignore; handled by determineCommandName
            case 'opts => Some(toStringList(value))
            case other => Some(List("-"+stringize(other), pathToString(value)))
        }

   protected def determineCommandName(options: Map[Symbol, Any]) = 
        options.getOrElse('command, name).toString()

    protected def buildCommandOptionsList(options: Map[Symbol, Any]) = 
        optionsProcessor.processOptionsToList(options).map(_.toString()).filter(_.length > 0)
    
    // hook for test overrides.
    protected def makeFilesLister() = new FilesFinder()

    protected def pathToString(value: Any) = value match {
        case seq:Seq[_] => Path(seq)
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

    protected def toCommandString(command: String, list: List[String]) = 
        list.foldLeft(command)(_ + " " + _ )
}

object ShellCommand {
    /**
     * Shell command specified as a string. Note that the string will be split on
     * whitespace into tokens. Use {@link Command.apply(options: (A,B)*)} if some of 
     * the "words" contain whitespace.
     */
    def apply[Symbol,Any](str: String): Result = {
        val list = str.split(" ").toList
        new ShellCommand(list.head).apply('command -> list.head, 'opts -> list.tail)
    }
}
