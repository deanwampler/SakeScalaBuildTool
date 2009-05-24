package sake.command.builtin

import sake.command.Command
import sake.util._

/**
 * Command for recursive invocations of sake (as a new process), usually in a different directory.
 */
class SakeCommand() extends JVMCommand("sake", Some(Map[Symbol,Any]())) {
    
    override def optionsPostFilter(options: Map[Symbol,Any]) = {
        val sakefile = options.getOrElse('f, options.getOrElse('file, "")) match {
            case "" => "sake.scala"
            case s => s
        }
        var targets = processTargets(options.getOrElse('targets, "all")).trim
        options - ('command, 'inputText, 'f, 'file, 'targets) + 
            ('command -> "scala", 'inputText -> (":load " + sakefile + "\nbuild(\"" + targets + "\")\n"))
    }
    
    protected def processTargets(x: Any): String = x match {
        case h :: t => processTargets(h) + " " + processTargets(t)
        case Nil => ""
        case str: String => str
        case sym: Symbol => sym.name
        case _ => x.toString
    }
} 
    