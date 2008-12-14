package sake.command

import java.lang._
import sake.util._
import sake.environment._

/**
 * Defining and running arbitrary shell commands. 
 * TODO Assumes a *nix-like OS.
 */
class ShellCommand(name: String, defaultOptions: Option[Map[Symbol,Any]]) 
    extends Command[Symbol,Any](name, defaultOptions) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name:String) = this(name, None)

    def apply[Symbol,Any](str: String): Result = {
        val list = str.split(" ").toList
        apply('command -> list.head, 'opts -> toStringListFromList(list.tail))
    }

    override def action(result: Result, options: Map[Symbol,Any]) = {
        val command = determineCommandName(options)
        val opts = buildCommandOptions(options)
        Log.log(Level.Notice, "shell: " + toCommandString(command, opts))
        if (Environment.environment.dryRun == true)
            result
        else
            new CommandRunner(command, opts).run()
    }
    
    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'command => None  // already handled.
            case 'opts    => Some(toStringList(value))
            case 'files   => value match {
                case f:Files   => Some(f())
                case l:List[_] => Some(makeFilesLister()(l.map(_.toString())))
                case x         => Some(makeFilesLister()(List(x.toString())))
            }
            case other    => Some(List("-"+stringize(other), pathToString(value)))
        }
    
    protected var optionProcessors: List[(Symbol, Any) => Option[List[String]]] = List(optionProcessor _)

    protected def determineCommandName(options: Map[Symbol,Any]) = options.get('command) match {
        case None => name
        case Some(x) => stringize(x)
    }

    // Builds up the list in reverse order (due to list semantics), then reverses.
    protected def buildCommandOptions(options: Map[Symbol,Any]): List[String] =
        options.foldLeft(List[String]()) { (list, k_v) => 
            evaluateOption(k_v._1, k_v._2) ::: list
        }.reverse

    private def evaluateOption(key: Symbol, value: Any): List[String] = {
        for {
            f <- optionProcessors
            list = f(key, value)
        } list match {
            case Some(s) => return s.reverse
            case None =>
        }
        Nil
    }
    
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
        
    protected def makeFilesLister() = new Files()
}
