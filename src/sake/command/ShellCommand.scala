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

    def apply[Symbol,B](str: String): Result = {
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
    
    protected def determineCommandName(options: Map[Symbol,Any]) = options.get('command) match {
        case None => name
        case Some(x) => stringize(x)
    }

    // Builds up the list in reverse order (due to list semantics), then reverses.
    protected def buildCommandOptions(options: Map[Symbol,Any]):List[String] =
        options.foldLeft(List[String]()) { (list, k_v) => 
            val value = k_v._2
            k_v._1 match {
                case 'recursive => "-r"  :: list
                case 'force     => "-f"  :: list
                case 'cp        => pathToString(value) :: "-cp" :: list
                case 'classpath => pathToString(value) :: "-cp" :: list
                case 'opts      => toStringList(value).reverse ::: list
                case 'command   => list  // already handled.
                case 'files     => toStringList(value).reverse ::: list // TODO: expand to real file names
                case other      => pathToString(value) :: "-"+stringize(other) :: list
            }
        }.reverse
    
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
