package sake.command.builtin

import sake.command._
import sake.util._

trait Commands {

    var classpath: List[String] = Nil

    val files = new Files()

    def mkdirs(paths: String*): Unit = paths.foreach { path => 
        val dir = File(path)
        if (! dir.exists) {
            dir.mkdirs match {
                case true =>
                case false => Exit.error("Could not create directory \""+path+"\".")
            }
        }
    }
    
    def mkdir(path: String) = mkdirs(path)

    val sh = new ShellCommand("")

    val echo = new EchoCommand()
    
    // TODO: OS specific default: "rm".
    val remove           = new RemoveShellCommand("rm")
    val remove_force     = new RemoveShellCommand("rm", 'force -> true)
    val remove_recursive = new RemoveShellCommand("rm", 'recursive -> true)
    
    val scala  = new JVMShellCommand("scala")
    val scalac = new JVMShellCommand("scalac", 'files -> ".")
    val spec   = new JVMShellCommand("scala") {
        private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
            key match {
                case 'specs => {
                    val specsToRun = List(stringize(value))
                    if (specsToRun.length == 0)
                        Exit.error("spec: No specification files were given!")
                    Some(specsToRun)
                }
                case _ => None
            }
        
        optionProcessors ::= optionProcessor _
    }

    val java   = new JVMShellCommand("java")
    val javac  = new JVMShellCommand("javac", 'files -> ".")
}
