package sake.command.builtin

import sake.command._

trait Commands {

    var classpath: List[String] = Nil

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
                case 'specs => Some(List(stringize(value)))
                case _      => None
            }
        
        optionProcessors ::= optionProcessor _
    }

    val java   = new JVMShellCommand("java")
    val javac  = new JVMShellCommand("javac", 'files -> ".")
}
