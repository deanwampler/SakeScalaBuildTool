package sake.command.builtin

import sake.environment._
import sake.util._

trait Commands {

    val echo = new ShellCommand("echo") {
        override val requiredOptions = List[Symbol]('message)        
    }

    val remove = new ShellCommand("rm", 'rf -> "-rf") {
        override val knownOptions: Option[List[Symbol]] = Some(List('files, 'rf))
        override val requiredOptions: List[Symbol] = List('files)
    }

    val scala = new ShellCommand("scala", 'classpath -> Environment.classpath) {
        override val knownOptions: Option[List[Symbol]] = Some(List('opts, 'classpath))
    }
    
    val scalac = new ShellCommand("scalac", 'files -> ".", 'classpath -> Environment.classpath) {    
        override val knownOptions = Some(List('opts, 'classpath, 'files))
    }

    val spec = new ShellCommand("spec", 'files -> ".", 'classpath -> Environment.classpath) {    
        override val knownOptions = Some(List('opts, 'classpath, 'files))
    }

    val sh = new ShellCommand("sh") {
        override val requiredOptions = List[Symbol]('command)        
    }
}
