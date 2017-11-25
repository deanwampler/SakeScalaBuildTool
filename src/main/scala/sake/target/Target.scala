package sake.target

import sake.util.{Dedup, Exit}
import sake.command.{Result, Passed, Failed}

/**
 * A Target has zero or more dependencies and a sequence of actions (which can be empty).
 */
case class Target[+T] protected (
  value: T,
  dependencies: Seq[Target[_]],
  actions: Seq[Target.Action]) {

  /** Used as a key. We hide the implementation. */
  def name: String = value.toString

  def apply(action: Target.Action): Target[T] = copy(actions = actions :+ action)

  def build(context: Target.Context): Result = {
    try {
      Target.buildActions(context, actions)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${value}", ex)
    }
  }
}

object Target {

  case class Context(target: Target[_], properties: Map[String, Any]) {
    def apply(key: String): Any = properties(key)
    def get(key: String): Option[Any] = properties.get(key)
  }

  type Action = Context => Result

  /** Track all definitions for a target */
  def targets: Map[String, Target[_]] = allTargets

  protected var allTargets: Map[String, Target[_]] = Map.empty

  /** Example: target(value, dependencies = Seq(dep1, dep2, ...), actions = Seq(act1, ...)) */
  def apply[T](value: T, dependencies: Seq[Target[_]], actions: Seq[Action]): Target[T] =
    makeTarget(value, dependencies, actions)

  /** Example: target(value -> Seq("a", "b"), actions = Seq(act1, ...)) */
  def apply[T](value_dependencies: (T, Seq[Target[_]]), actions: Seq[Action]): Target[T] =
    makeTarget(value_dependencies._1, value_dependencies._2, actions)

  /**
   * Several names, with the same dependencies and actions.
   * Example: target(values = Seq(name1, ...), dependencies = Seq(dep1, ...), actions = Seq(act1, ...))
   */
  def apply[T](values: Seq[T], dependencies: Seq[Target[_]], actions: Seq[Action]): Vector[Target[T]] =
    values.map(v => makeTarget(v, dependencies, actions)).toVector

  /**
   * Several values of the same type, which share the same dependencies and actions.
   * Example: target(values_dependencies = (Seq(name1, ...) -> Seq(dep1, ...), actions = Seq(act1, ...))
   */
  def apply[T](values_dependencies: (Seq[T], Seq[Target[_]]), actions: Seq[Action]): Vector[Target[T]] = {
    val values = values_dependencies._1
    val dependencies = values_dependencies._2
    values.map(v => makeTarget(v, dependencies, actions)).toVector
  }

  import sake.files.File

  // These conversions are primarily used to autoconvert dependencies specified as
  // strings, symbols, files, etc.

  implicit def toTarget(name: String) = apply[String](name, Nil, Nil)
  implicit def toTarget(name: Symbol) = apply[String](name.toString, Nil, Nil)
  implicit def toTarget(file: sake.files.File) = apply[File](file, Nil, Nil)

  def buildActions(context: Context, actions: Seq[Action]): Result = {
    def b(lastResult: Result, acts: Seq[Action]): Result = acts match {
      case Nil => lastResult
      case head +: tail =>
        val result = head(context)
        if (result.passed) b(result, tail) else result
    }
    b(Passed.default, actions)
  }

  protected def makeTarget[T](value: T, dependencies: Seq[Target[_]], actions: Seq[Action]): Target[T] =
    registerTarget(new Target[T](value, Dedup(dependencies), actions))

  protected def registerTarget[T](target: Target[T]): Target[T] =
    allTargets.get(target.name) match {
      case None =>
        allTargets += (target.name -> target)
        target
      case Some(t2) =>
        val tt = Target.merge(target, t2)
        allTargets += (target.name -> tt)
        tt
    }

  /*
   * Merge two targets. The dependencies of `t` are taken first, with duplicates removed.
   * Note that type of the second target is `Target[Any]`, because this method is called
   * by `registerTarget`, which doesn't know the more specific type.
   * A new action is synthesized from the two actions of the input targets, with `t` going first.
   */
  protected def merge[T](t: Target[T], tany: Target[Any]): Target[T] = {
    validateMerge(t, tany)
    val deps = Dedup(t.dependencies ++ tany.dependencies)
    new Target[T](t.value, deps, t.actions ++ tany.actions)
  }

  // TODO. We exit if unable to merge, rather than return a Result.
  protected def validateMerge[T](t: Target[T], tany: Target[Any]) =
    if (t.value != tany.value)
      Exit.error("Target.merge() called with two targets that don't have the same value: "+t.value+" vs. "+tany.value)

}
