package sake.command.builtin

class ScalaCommand(name: String, defaultOptions: Map[Any, Any]) 
    extends Command[String](name, defaultOptions) {
}

object ScalaCommand {
    val scala = new ScalaCommand("scala", Map('command -> ""))
}
