package sake.command.builtin

import sake.environment._

object BuiltinCommands {
    val scala = new ShellCommand("scala", 'classpath -> Environment.classpath) {

        override val knownOptions: Option[List[Symbol]] = Some(List('help, 'opts, 'classpath))
    }
    
    val scalac = new ShellCommand("scalac", 'files -> ".", 'classpath -> Environment.classpath) {    

        override val knownOptions: Option[List[Symbol]] = Some(List(
            'help, 'opts, 'classpath, 'files))
    }

    val sh = new ShellCommand("sh") {
        override val requiredOptions = List[Symbol]('command)        
    }
}
