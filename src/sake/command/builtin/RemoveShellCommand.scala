package sake.command.builtin

import sake.command.ShellCommand

class RemoveShellCommand(name: String, options: Option[Map[Symbol,Any]]) extends ShellCommand(name, options) {
    
    def this(name: String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name: String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name: String) = this(name, None)

    override val requiredOptions: List[Symbol] = List('files)
    
    // TODO: OS specific defaults for: "recursive" and "force".
    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'recursive => Some(List("-rf"))
            case 'force     => Some(List("-f"))
            case _          => None
        }
    
    optionProcessors ::= optionProcessor _

}

