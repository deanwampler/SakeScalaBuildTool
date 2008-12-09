package sake.target

class TargetManager {
    import scala.collection.mutable.HashMap
    
    /**
     * The map of user defined targets, with the target names as the keys.
     */
    private var targets = HashMap[Symbol, Target]()
    
    def allTargets() = targets.clone()

    def addTarget(t: Target) = targets.put(t.name, t)

    def mergeTarget(t: Target):Unit = {
        // TODO
        throw new Exception("Not implemented yet.")
    }

    def removeTarget(t: Target) = targets.removeKey(t.name)

    def getTarget(name: Symbol) = targets.get(name)

    def reset() = targets.clear()
}
