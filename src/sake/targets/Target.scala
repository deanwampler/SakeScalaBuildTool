package sake.target

import scala.collection.immutable._
import sake.util._

/** Basic target class that can have only dependencies not an action.
 * The dependencies are handles as private vars so they can be updated. It would
 * be nice to simply create a new Target when the dependencies are updates. Unfortunately,
 * you can't clone a by-name value, which is used in the TargetWithAction subclass.
 */
class Target(val name: Symbol, deps: List[Symbol]) {
    private var deps_ = SymbolUtil.removeDuplicates(deps)
    
    def dependencies = deps_
    
    def build() = {}
}


class TargetWithAction(name: Symbol, deps: List[Symbol], action: => Unit) 
    extends Target(name, deps){
    
    override def build() = {
        try {
            action
        } catch {
            case th:Throwable => Exit.error("target \""+name+"\" failed: ", th)
        }
    }
}

object Target {
    def apply(name:Symbol) = new Target(name, Nil)
    def apply(name:Symbol, dependencies:List[Symbol]) = new Target(name, dependencies)
    def apply(name:Symbol, dependencies:List[Symbol], action: => Unit) = 
        new TargetWithAction(name, dependencies, action)

    def merge(t1: Target, t2: Target) = {
        if (t1.name != t2.name)
            Exit.error("Target.merge() called with two targets that don't have the same name: "+t1.name+", "+t2.name)
        doMerge(t1, t2)
    }
    
    private def doMerge(t1: Target, t2: Target) = t1 match {
        case t:TargetWithAction => t2 match {
            case t:TargetWithAction => Exit.error("A target can have only one action. Target "+t2.name
                +"has at least two actions defined.")
            case _ => mergeDepsAndReturn(t1, t1, t2)
        }
        case _ => mergeDepsAndReturn(t2, t1, t2)  // merge dependencies in the declaration order!
    }
    
    private def mergeDepsAndReturn(returnedTarget: Target, t1: Target, t2: Target) = {
        returnedTarget.deps_ = SymbolUtil.removeDuplicates(t1.deps_ ::: t2.deps_)
        returnedTarget
    }
}