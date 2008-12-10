package sake.command

import java.lang._
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
        val cmd = "echo "+command + " " + buildCommandString(options)
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
            case 'opts      => value.toString()
            case 'f         => value.toString()
            case 'r         => value.toString()
            case 'rf        => value.toString()
            case 'recursive => value.toString()
            case 'force     => value.toString()
            case 'classpath => "-cp \""+doPath(value)+"\""
            case 'files     => value.toString() //new Files(value).toString()
            case other      => "-"+other.name+" \""+doPath(value)+"\""
        }
        list.toList.foldLeft("")(_ + " " + _ )
    }

    protected def runCmd(cmd: String) = {
        val process = Runtime.getRuntime().exec(cmd)
        process.waitFor() match {
            case 0 => new Passed()
            case i:Int => new Failed(Some(i))
        }
    }
    
    private def doPath(path: Any) = path match {
        case seq:Seq[_] => Path(seq)
        case _ => path.toString()  // "Hail Mary..."
    }
}
