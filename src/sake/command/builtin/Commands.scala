package sake.command.builtin

import sake.util._
import sake.command.ShellCommand

trait Commands {

    var classpath: List[String] = Nil
    
    val echo = new ShellCommand("echo") {
        override val requiredOptions = List[Symbol]('message)        
    }

    private val remove_opts = Some(List('files, 'rf, 'recursive, 'force))
    val remove = new ShellCommand("rm") {
        override val knownOptions: Option[List[Symbol]] = remove_opts
        override val requiredOptions: List[Symbol] = List('files)
    }

    val remove_force = new ShellCommand("rm", 'f -> "-f") {
        override val knownOptions: Option[List[Symbol]] = remove_opts
        override val requiredOptions: List[Symbol] = List('files)
    }

    val remove_recursive = new ShellCommand("rm", 'rf -> "-rf") {
        override val knownOptions: Option[List[Symbol]] = remove_opts
        override val requiredOptions: List[Symbol] = List('files)
    }

    val scala = new ShellCommand("scala", 'classpath -> classpath) {
        override val knownOptions: Option[List[Symbol]] = Some(List('opts, 'classpath))
    }
    
    val scalac = new ShellCommand("scalac", 'files -> ".", 'classpath -> classpath) {    
        override val knownOptions = Some(List('opts, 'classpath, 'files))
    }

    val spec = new ShellCommand("spec", 'files -> ".", 'classpath -> classpath) {    
        override val knownOptions = Some(List('opts, 'classpath, 'files))
    }

    val sh = new ShellCommand("") {
        override val requiredOptions = List[Symbol]('command)        
    }
}
