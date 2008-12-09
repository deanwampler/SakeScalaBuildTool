package sake.command.builtin

class ShellCommand(name: String, defaultOptions: Option[Map[Symbol,Any]]) 
    extends Command[Symbol,Any](name, defaultOptions) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    def this(name:String) = this(name, None)

    val command = name
    
    override def action(result: Result, options: Map[Symbol,Any]) = {
/*        import java.lang._
        
        var process = Runtime.getRuntime().exec(command + " " + buildCommandString(options))
        
        return if (exitValue() 
                  Returns the exit value for the subprocess.
        abstract  InputStream   getErrorStream() 
                  Gets the error stream of the subprocess.
        abstract  InputStream   getInputStream() 
                  Gets the input stream of the subprocess.
        abstract  OutputStream  getOutputStream() 
                  Gets the output stream of the subprocess.
*/
        result
    }
    
    protected def buildCommandString(options: Map[Symbol,Any]) = {
        val list = for { 
            (key, value) <- options
        } yield key match {
            case 'help      => "-help"
            case 'opts      => value.toString()
            case 'classpath => "-cp \""+value.toString()+"\""
            case 'files     => value.toString() //new Files(value).toString()
        }
        list.toList.foldLeft("")(_ + " " + _ )
    }
}
