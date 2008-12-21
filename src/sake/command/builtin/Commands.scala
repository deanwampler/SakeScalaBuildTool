package sake.command.builtin

import sake.command._
import sake.util._

trait Commands {

    var classpath: List[String] = Nil

    val files = new FilesFinder()

    def mkdirs(paths: String*): Unit = paths.foreach { path => 
        val dir = File(path)
        if (dir.exists == false)
            if (dir.mkdirs == false)
                Exit.error("Could not create directory \""+path+"\".")
    }
    
    def mkdir(path: String) = mkdirs(path)

    def delete(paths: String*): Unit = paths.foreach { path =>
        if (File(path).delete == false)
            Exit.error("Could not delete \""+path+"\".")
    }  

    def deleteRecursively(paths: String*): Unit = paths.foreach { path =>
        if (File(path).deleteRecursively == false)
            Exit.error("Could not delete \""+path+"\" recursively.")
    }  
    
    val sh = new ShellCommand("")

    val echo = new EchoCommand()

    val specs = new SpecCommand()
    
    val scala  = new JVMCommand("scala")
    val scalac = new JVMCommand("scalac", 'files -> ".")
    val specJVM = new JVMCommand("scala") {
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
        
        optionsProcessor.addProcessor(optionProcessor _)
    }

    val java   = new JVMCommand("java")
    val javac  = new JVMCommand("javac", 'files -> ".")
}
