package sake.command.builtin

import sake.command._
import sake.util._
import sake.util.Path._

trait Commands {

    var classpath = Path()

    val files = new FilesFinder()

    def mkdirs(paths: String*): Unit = mkdirs(paths.toList)
    
    def mkdirs(paths: List[String]): Unit = paths foreach { path => 
        val dir = makeFile(path)
        if (dir.exists == false)
            if (dir.mkdirs == false)
                Exit.error("Could not create directory \""+path+"\".")
    }
    
    def mkdir(path: String) = mkdirs(path)

    def delete(paths: String*): Unit = delete(paths.toList)

    def delete(paths: List[String]): Unit = paths foreach { path =>
        val file = makeFile(path)
        if (file.exists && file.delete == false)
            Exit.error("Could not delete \""+path+"\".")
    }  

    def deleteRecursively(paths: String*): Unit = deleteRecursively(paths.toList)

    def deleteRecursively(paths: List[String]): Unit = paths foreach { path =>
        val file = makeFile(path)
        if (file.exists && file.deleteRecursively == false)
            Exit.error("Could not delete \""+path+"\" recursively.")
    }
    
    def fail(): Unit = fail("Build failed!")

    def fail(message: String): Unit = Exit.error(message)
    
    protected def makeFile(path: String) = File(path)
    
    /**
     * Command for recursive invocations of sake (as a new process), usually in a different directory.
     */
    val sakecmd = new SakeCommand()
    
    /**
     * Use "sh" for invoking a shell command with a command string.
     */
    def sh(command: String) = ShellCommand(command)

    /**
     * Use "shell" for invoking a shell command with key-value pairs.
     */
    val shell = new ShellCommand("shell", Map('command -> "shell", 'opts -> ""))

    val echo = new EchoCommand()

    val scala  = new JVMCommand("scala")
    val scalac = new JVMCommand("scalac", 'files -> ".")

    val specs  = new SpecCommand()

    val java   = new JVMCommand("java")
    val javac  = new JVMCommand("javac", 'files -> ".")
}
