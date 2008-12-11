package sake.command

import java.lang._
import scala.actors._ 
import scala.actors.Actor._ 
import sake.util._
import sake.environment._

class ShellCommand(name: String, defaultOptions: Option[Map[Symbol,Any]]) 
    extends Command[Symbol,Any](name, defaultOptions) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name:String) = this(name, None)

    val command = name
    
    override def action(result: Result, options: Map[Symbol,Any]) = {
        val cmd = command + " " + buildCommandString(options)
        Log.log(Level.Notice, "shell: "+cmd)
        if (Environment.environment.dryRun == true)
            result
        else
            runCmd(cmd)
    }
    
    protected def buildCommandString(options: Map[Symbol,Any]) = {
        val list = for { 
            (key, value) <- options
        } yield key match {
            // TODO CLEAN UP.
            case 'f         => "-f"
            case 'r         => "-r"
            case 'rf        => "-rf"
            case 'recursive => "-r"
            case 'force     => "-f"
            case 'cp        => "-cp "+valueToString(value)
            case 'classpath => "-cp "+valueToString(value)
            case 'opts      => value.toString()
            case 'command   => value.toString()
            case 'files     => valueToString(value) // TODO: expand to real file names
            case other      => "-"+stringize(other)+" "+valueToString(value)
        }
        toCommandString(list.toList)
    }

    protected def valueToString(value: Any) = value match {
        case seq:Seq[_] => Path(seq)
        case p:Product  => p.toString()  // TODO: wrap each item in "..."?
        case _ => value.toString()
    }

    protected def runCmd(cmd: String):Result = {
        actor {
            var shell = new sake.util.actors.ShellActor()
            shell.start()
            shell ! (cmd, self)
            loop {
                react {
                    case r: Result => {println("got: "+r); return(r)}
                    case x: Any => println(x.toString())
                }
            }
        }
        new Passed()  // never gets here, but makes compiler happy ;)
    }
    
    protected def stringize(item: Any) = item match {
        case s:String => s
        case s:Symbol => s.name // not toString()
        case _  => item.toString()
    }
    
    protected def toCommandString(list: List[String]) = list.foldLeft("")(_ + " " + _ )
}
