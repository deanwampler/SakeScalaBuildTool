package sake

class Target(val name: Symbol, val dependencies: List[Symbol], action: => Unit) {
    def invoke() = action
}
