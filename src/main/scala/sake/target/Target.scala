package sake.target

import sake.util.{Dedup, Exit}
import sake.command.{Result, Passed, Failed}

/**
 * A Target has zero or more dependencies and a sequence of actions (which can be empty).
 */
case class Target protected (name: String, dependencies: Seq[String], actions: Seq[Target.Action]) {

  def apply(action: Target.Action): Target = copy(actions = actions :+ action)

  def build(context: Target.Context): Result = {
    try {
      Target.buildActions(context, actions)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${name}", ex)
    }
  }
}

object Target {

  type Context = Map[String, Any]
  type Action = Context => Result

  /** Track all definitions for a target */
  def targets: Map[String, Target] = allTargets

  protected var allTargets: Map[String, Target] = Map.empty

  /** Example: target(name, dependencies = Seq(dep1, dep2, ...), actions = Seq(act1, ...)) */
  def apply(name: String, dependencies: Seq[String] = Vector.empty, actions: Seq[Action] = Vector.empty): Target =
    makeTarget(name, dependencies, actions)

  /** Example: target('name -> Seq('a, 'b), actions = Seq(act1, ...)) */
  def apply(name_dependencies: (String, Seq[String]), actions: Seq[Action] = Vector.empty): Target =
    makeTarget(name_dependencies._1, name_dependencies._2, actions)

  /**
   * Several names, with the same dependencies and actions.
   * Example: target(names = Seq(name1, ...), dependencies = Seq(dep1, ...), actions = Seq(act1, ...))
   */
  def apply(names: Seq[String], dependencies: Seq[String] = Vector.empty, actions: Seq[Action] = Vector.empty): Vector[Target] =
    names.map(n => makeTarget(n, dependencies, actions)).toVector

  /**
   * Several names, with the same dependencies and actions.
   * Example: target(names_dependencies = (Seq(name1, ...) -> Seq(dep1, ...), actions = Seq(act1, ...))
   */
  def apply(names_dependencies: (Seq[String], Seq[String]), actions: Seq[Action] = Vector.empty): Vector[Target] = {
    val names = names_dependencies._1
    val dependencies = names_dependencies._2
    names.map(n => makeTarget(n, dependencies, actions)).toVector
  }

  def buildActions(context: Context, actions: Seq[Action]): Result = {
    def b(lastResult: Result, acts: Seq[Action]): Result = acts match {
      case Nil => lastResult
      case head +: tail =>
        val result = head(context)
        if (result.passed) b(result, tail) else result
    }
    b(Passed.default, actions)
  }

  protected def makeTarget(name: String, dependencies: Seq[String], actions: Seq[Action]): Target = {
    val t = new Target(name, Dedup(dependencies), actions)
    val returnT = allTargets.get(name) match {
      case None =>
        allTargets += (name -> t)
        t
      case Some(t2) =>
        val tt = Target.merge(t, t2)
        allTargets += (name -> tt)
        tt
    }
    returnT
  }

  /**
   * Merge two targets. The dependencies of t1 are taken first, with duplicates removed.
   * A new action is synthesized from the two actions of the input targets, with t1 going first.
   */
  protected def merge(t1: Target, t2: Target): Target = {
    validateMerge(t1, t2)
    val deps = Dedup(t1.dependencies ++ t2.dependencies)
    new Target(t1.name, deps, t1.actions ++ t2.actions)
  }

  protected def validateMerge(t1: Target, t2: Target) =
    if (t1.name != t2.name)
      Exit.error("Target.merge() called with two targets that don't have the same name: "+t1.name+", "+t2.name)

}
