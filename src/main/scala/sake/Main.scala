package sake

import test._

/** 
 * Prototype of an alternative way of invoking Sake. Currently, the bin/sake 
 * script uses a hack where it pipes the build script into the scala interpreter.
 * This object uses Josh Suereth's embedded interpreter example to load the script
 * from within Scala instead. At this time, it works fine, except that it puts
 * the build in interactive mode after loading the build file, even if you don't
 * specify the interactive option. The user must type 'build("target_names")'
 * explicitly, followed by ":quit", when done. Obviously not what I want....
 */ 
object Main {
  val helpMessage = 
  """usage: scala -classpath ... sake.Main [-h] [-f sake.scala] [targets]
  where:
      -classpath ...  Must include all the jars in $SAKE_HOME/lib 
                      (The $SAKE_HOME/bin/sake script handles this).
      -h              This help message (also "--h", "-help", "--help", etc. are allowed)
      -i              Interactive mode. After loading the build file, it puts you at the scala
                      command prompt. Use commands like "build('all)" to build targets.
      -f build_file   The build file name, where "sake.scala" is the default.
      targets         What to build; defaults to "all", unless in interactive mode, in which
                      case nothing is built by default. If targets are specified, they are
                      built after loading the build file and before presenting the scala prompt.
  """

  def main(args: Array[String]) = {
    val options = processArgs(args)
    val interpreter = new InterpreterWrapper() {
      def prompt = "sake> "
      def welcomeMsg = """Sake build tool."""
      def helpMsg = helpMessage
      autoImport("sake.Project._")
      addScriptFile(options.get("file").get.asInstanceOf[String])
    }  
    interpreter.startInterpreting
    options.get("interactive") match {
      case None => 
        options.get("targets").get match {
          case list: List[_] => list.foreach(x => Project.build(x.toString))
          case _ =>
        }
        exit(0)
      case _ =>
    }
  }

  protected def processArgs(args: Array[String]) = {
    val options = new scala.collection.mutable.HashMap[String,Any]
    options.update("file", "sake.scala")
    options.update("targets", Nil)
    var i = 0
    while (i < args.length) {
      args(i) match { 
    		case "-h" | "--h" | "-help" | "--help" => println(helpMessage); exit(0)
    		case "-i" | "--i" | "-interactive" | "--interactive" => options.update("interactive", true)
    		case "-f" | "--f" | "-file" | "--file" => i += 1; options.update("file", args(i))
    		case x => 
    		  val list = options.get("targets").get.asInstanceOf[List[_]]
    		  options.update("targets", x :: list)
      }
      i += 1
    }
    options.get("targets") match {
      case None => options.get("interactive") match {
        case None => options.update("targets", List("all"))
        case _ =>
      }
      case _ =>
    }
    options
  }
}