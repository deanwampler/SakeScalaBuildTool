package sake.command

import java.io._
import sake.util.Exit

class CommandRunner(val command: String, val arguments: List[String], val environment: Option[Map[Any, Any]]) {

    def this(command: String, arguments: List[String]) = this(command, arguments, None)
    def this(command: String) = this(command, Nil)
    
    val processBuilder = new ProcessBuilder()
    processEnvironment(processBuilder)

    if (command.length == 0)
        Exit.error("Must specify a non-empty command name")
        
    def run() = {
        processBuilder.redirectErrorStream(true)
        processBuilder.command(toJavaList(command :: arguments))
        val process = processBuilder.start()
        val out = new BufferedReader(new InputStreamReader(process.getInputStream()))
        processCommandOutput(out)
        getStatus(process)
    }

    var formatOutputLine: (String, String) => (String) = { (cmdName, line) => cmdName + ": " + line }
    var outputPrintStream = Console.out
    
    protected def writeOutput(line: String) = outputPrintStream.println(formatOutputLine(command,line))
    
    protected def processCommandOutput(out: BufferedReader): Unit = {
        var line = out.readLine()
        while (line != null) {
            writeOutput(line)
            line = out.readLine()
        }
    }

    protected def processEnvironment(pb: ProcessBuilder): ProcessBuilder = {
        var env = pb.environment()
        environment match {
            case None =>
            case Some(m:Map[_,_]) => m.foreach { key_value =>
                key_value._1 match {
                    case 'directory => pb.directory(new File(key_value._2.toString()))
                    case key => env.put(key.toString(), key_value._2.toString())
                }
            }
        }
        pb
    }
    
    private def toJavaList(list: List[_]) =
        list.foldLeft(new java.util.ArrayList[String]()) { (l, s) => l.add(s.toString()); l }

    private def getStatus(process: Process) = process.waitFor() match {
        case 0 => new Passed()
        case i:Int => new Failed(Some(i))
    }
}
