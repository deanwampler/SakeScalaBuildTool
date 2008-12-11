package sake.util.actors

import sake.util._
import sake.command._

import scala.actors._
import scala.actors.Actor._ 

class ShellActor extends Actor {
    def act() { 
        loop {
            react {
                case (command:String, sender:Actor) => runCommand(command, sender)
                case x: Any => exit(Failed(Some(2), Some("Unknown object sent to ShellActor! " + x)))
            }
        }
    }

    private def runCommand(command: String, sender: Actor) = {
        println("Starting command: "+command)
        import java.io._
        val pb = new ProcessBuilder()
        pb.redirectErrorStream(true)
        pb.command(toList(command))
        val process = pb.start()
        val out = new BufferedReader(new InputStreamReader(process.getInputStream()))
        out.readLine() match {
            case null => sender ! getStatus(process)
            case x => {println("x: "+x);sender ! x; sender ! getStatus(process)}
        }
    }

    private def toList(str: String) =
        str.split(" ").foldLeft(new java.util.ArrayList[String]()) { (list, s) => 
            list.add(s)
            list
        }
    
    private def getStatus(process: Process) = process.waitFor() match {
        case 0 => new Passed()
        case i:Int => new Failed(Some(i))
    }
}
