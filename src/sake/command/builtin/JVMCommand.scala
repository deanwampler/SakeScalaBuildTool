package sake.command.builtin

import sake.command.ShellCommand

class JVMCommand(name: String, options: Option[Map[Symbol,Any]]) extends ShellCommand(name, options) {

    def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))
    
    def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) = 
        this(name, Map[Symbol,Any](defaultOpt0)++defaultOpts)
    
    def this(name:String) = this(name, None)

    private def optionProcessor(key: Symbol, value: Any): Option[List[String]] = 
        key match {
            case 'class     => Some(List(stringize(value)))
            case 'cp        => Some("-cp" :: pathToString(value) :: Nil)
            case 'classpath => Some("-cp" :: pathToString(value) :: Nil)
            case _          => None
        }
    
    optionsProcessor.addProcessor(optionProcessor _)
}

