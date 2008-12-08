package sake.command.builtin

include sake.environment._

object BuiltinCommands {
    val scala = new Command[Symbol,Any]("scala")
    
    val scalac = new Command[Symbol,Any]("scalac", 'files -> ".", 'classpath -> classpath)

    val sh = new Command[Symbol,Any]("sh") {
        override val requiredOptions = List[Symbol]('command)        
    }
}
