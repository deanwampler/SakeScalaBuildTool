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
            case 'recursive => value match {
                case true  => Some(List("-rf"))
                case false => Some(List(""))
            }
            case 'force     => value match {
                case true  => Some(List("-f"))
                case false => Some(List(""))
            }
            case _          => None
        }
    
    optionProcessors ::= optionProcessor _

}

