package sake.command

import java.io.{File => JFile, Reader => JReader, StringReader => JStringReader, 
    InputStreamReader => JInputStreamReader, BufferedReader => JBufferedReader, 
    Writer => JWriter, BufferedWriter => JBufferedWriter, OutputStreamWriter => JOutputStreamWriter}
import sake.util._
import sake.environment._

class CommandRunner(val command: String, val arguments: List[String], val environment: Option[Map[Any, Any]]) {

    def this(command: String, arguments: List[String]) = this(command, arguments, None)
    def this(command: String) = this(command, Nil)
    
    protected var textInputForProcess:  Option[String] = None
    protected var fileInputForProcess:  Option[File] = None
    protected var fileOutputForProcess: Option[File] = None
    
    val processBuilder = new ProcessBuilder()
    processEnvironment(processBuilder)

    if (command.length == 0)
        Exit.error("Must specify a non-empty command name")
        
    def run() = {
        processBuilder.redirectErrorStream(true)
        processBuilder.command(toJavaArrayList(command :: arguments))
        val process = processBuilder.start()
        handleInputFor(process)
        handleOutputFor(process)
        getStatus(process)
    }

    protected def handleInputFor(process: Process): Unit = {
        val reader: JBufferedReader = textInputForProcess match {
            case Some(s) => new JBufferedReader(new JStringReader(s))
            case None => fileInputForProcess match {
                case None => return
                case Some(f) => new JBufferedReader(f.reader)
            }
        }
        val writer = new JBufferedWriter(new JOutputStreamWriter(process.getOutputStream()))
        while (true) {
            reader.readLine() match {
                case null => {
                    writer.flush()
                    writer.close()
                    return
                }
                case line => {
                    writer.write(line + Environment.environment.lineSeparator)
                }
            }
        }
    }
    
    protected def handleOutputFor(process: Process) = {
        val writer: JWriter = fileOutputForProcess match {
            case None => new JBufferedWriter(new JOutputStreamWriter(Console.out))
            case Some(f) => f.writer
        }
        val out = new JBufferedReader(new JInputStreamReader(process.getInputStream()))
        processCommandOutput(writer, out)
    }
    
    protected def processCommandOutput(writer: JWriter, out: JBufferedReader): Unit = {
        var line = out.readLine()
        while (line != null) {
            writer.write(line + Environment.environment.lineSeparator)
            line = out.readLine()
        }
        writer.flush()
    }

    protected def processEnvironment(pb: ProcessBuilder): ProcessBuilder = {
        var env = pb.environment()
        environment match {
            case None =>
            case Some(m:Map[_,_]) => m.foreach { key_value =>
                key_value._1 match {
                    case 'directory  => pb.directory(new JFile(key_value._2.toString()))
                    case 'inputText  => textInputForProcess  = Some(key_value._2.toString())
                    case 'inputFile  => fileInputForProcess  = Some(determineFile(key_value._2))
                    case 'outputFile => fileOutputForProcess = Some(determineFile(key_value._2))
                    case key => env.put(key.toString(), key_value._2.toString())
                }
            }
        }
        if (textInputForProcess != None && fileInputForProcess != None)
            Exit.error("Can't specify both an input file and input text for a subprocess.")
        pb
    }
    
    protected def determineFile(value: Any) = value match {
        case f:File => f
        case x      => File(x.toString())
    }
    
    private def toJavaArrayList(list: List[_]) =
        list.foldLeft(new java.util.ArrayList[String]()) { (l, s) => l.add(s.toString()); l }

    private def getStatus(process: Process) = process.waitFor() match {
        case 0 => new Passed()
        case i:Int => new Failed(Some(i))
    }
}
