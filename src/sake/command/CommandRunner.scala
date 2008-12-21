package sake.command

import java.io._

class CommandRunner(val command: String, val options: List[String]) {

    def this(commandLine: List[String]) = this(commandLine.head, commandLine.tail)
    
    def run() = {
        val pb = new ProcessBuilder()
        pb.redirectErrorStream(true)
        pb.command(toJavaList(command :: options))
        val process = pb.start()
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

    private def toJavaList(list: List[_]) =
        list.foldLeft(new java.util.ArrayList[String]()) { (l, s) => l.add(s.toString()); l }

    private def getStatus(process: Process) = process.waitFor() match {
        case 0 => new Passed()
        case i:Int => new Failed(Some(i))
    }
}
