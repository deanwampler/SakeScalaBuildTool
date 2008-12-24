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
        var targets = options.getOrElse('targets, "") match {
            case "" => "all"
            case s => s
        }
        options - ('command, 'input, 'f, 'file, 'targets) + 
            ('command -> "scala", 'input -> (":load " + sakefile + "\nbuild(\"" + targets + "\")\n"))
    }
} 
    