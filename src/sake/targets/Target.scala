package sake.targets

import sake.util._

class Target(val name: Symbol, val dependencies: List[Symbol], action: => Unit) {

    def build() = action
    
    def this(name:Symbol) = this(name, Nil, {})
    def this(name:Symbol, dependencies:List[Symbol]) = this(name, dependencies, {})
}

object Target {
    
    def makeTarget(names: Any, deps: Any): TargetGroup = names match {
        case head :: tail => makeTarget(head, deps) ::: makeTarget(tail, deps)
        case Nil => new TargetGroup()
        case _ => {
            val name = SymbolUtil.toSymbol(names)
            new TargetGroup(new Target(name, SymbolUtil.toList(deps)))
        }
    }
}
