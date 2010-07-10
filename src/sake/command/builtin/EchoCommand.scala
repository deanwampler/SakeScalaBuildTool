package sake.command.builtin

import sake.command._
import sake.util._

/**
 * Log a string at the specified level.
 * You can also use "println". use EchoCommand if you want the output to be logged consistently
 * with the rest of the build output.
 */
class EchoCommand(val defaultLevel: Level.Value) extends Command[Symbol,Any]("echo") {
    
    def this() = this(Level.Notice)
    
    def apply(str: String): Result = {
        val list = str.split(" ").toList
        apply('words -> str.split(" ").toList)
    }

    override def action(options: Map[Symbol,Any]) = {
        val words = options.get('words) match {
            case None => ""
            case Some(x) => x match {
                case list:List[_] => list.map(_.toString()).reduceRight(_+" "+_)
                case y => y.toString()
            }
        }
        val level = options.get('level) match {
            case None => defaultLevel
            case Some(l) => l match {
                case lev:Level.Value => lev
                case _ => Exit.error("Invalid value for level ("+l+"). Must be a Log Level.")
            }
        }
        Log.log(level, words) 
        new Passed()
    }
    
    override val requiredOptions: List[Symbol] = List('words)
}

