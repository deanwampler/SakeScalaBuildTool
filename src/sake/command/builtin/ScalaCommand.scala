package sake.command.builtin
/*
private class ScalaCommand(name: String, defaultOptions: Map[Symbol, Any]) 
    extends Command(name, defaultOptions) {
        
    override val requiredOptions = List('command)
}
*/
object BuiltinCommands {
    val scala = new Command[Symbol,Any]("scala") {
        override val requiredOptions = List[Symbol]('command)        
    }
}
