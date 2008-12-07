package sake.targets

class Target(val name: Symbol, val dependencies: List[Symbol], action: => Unit) {

    def invoke() = action
    
    def this(name:Symbol, dependencies:List[Symbol]) = this(name, dependencies, {})
}
