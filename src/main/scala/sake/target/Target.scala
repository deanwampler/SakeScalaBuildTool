package sake.target

import sake.util.Exit
import sake.command.{Result, Passed, Failed}
import sake.context.Context

/**
 * A Target has zero or more dependencies and a sequence of actions (which can be empty).
  @param value a String, Symbol, File, etc. used as the kind of target.
  @param name a string used as a key to identify and locate the target.
  @param dependencies that are built first, if any.
  @param actions are the functions to build the target.
  @param preActions are the functions executed before the main actions, if any. This exists because targets can be merged, and it's a way to enforce ordering.
  @param postActions are the functions executed after the main actions, if any. This exists because targets can be merged, and it's a way to enforce ordering.
 */
case class Target[T] protected (
  name: String,
  value: T,
  dependencies: Seq[Target[_]] = Vector.empty,
  actions: Seq[Context[T] => Result] = Vector.empty,
  preActions: Seq[Context[T] => Result] = Vector.empty,
  postActions: Seq[Context[T] => Result] = Vector.empty) {

  /** Add dependencies */
  def dependencies (dep1: Target[_], deps: Target[_]*) = copy(dependencies = dependencies ++ (dep1 +: deps))
  def dependencies (deps: Seq[Target[_]]) = copy(dependencies = dependencies ++ deps)
  def deps (dep1: Target[_], deps: Target[_]*) = copy(dependencies = dependencies ++ (dep1 +: deps))
  def deps (deps: Seq[Target[_]]) = copy(dependencies = dependencies ++ deps)
  def apply (dep1: Target[_], deps: Target[_]*) = copy(dependencies = dependencies ++ (dep1 +: deps))
  def apply (deps: Seq[Target[_]]) = copy(dependencies = dependencies ++ deps)

  /** Add an action to the target. This is NOT the method to call to build a target. */
  def apply(action: Context[T] => Result): Target[T] = copy(actions = actions :+ action)

  /** Prepend an action to the preActions. */
  def prepend(action: Context[T] => Result): Target[T] = copy(preActions = action +: preActions)

  /** Append an action to the postActions. */
  def append(action: Context[T] => Result): Target[T] = copy(postActions = postActions :+ action)

  def build(context: Context[T]): Result = {
    try {
      buildActions(context, preActions ++ actions ++ postActions)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${value}", ex)
    }
  }

  protected def buildActions(context: Context[T], actions: Seq[Context[T] => Result]): Result = {
    def b(lastResult: Result, acts: Seq[Context[T] => Result]): Result = acts match {
      case Nil => lastResult
      case head +: tail =>
        val result = head(context)
        if (result.passed) b(result, tail) else result
    }
    b(Passed.default, actions)
  }

  /*
   * Merge a second target into this one. The dependencies of this target are taken
   * first, with duplicates removed. The new sequence of actions will be as follows
   * (note the postActions):
   *   this.preActions ++ that.preActions ++ this.actions ++ that.actions ++ that.postActions ++ this.postActions
   * Because of the way this is used, we don't know the two type parameters of the
   * second target, so we "guess". Also, we don't want to constrain TD anyway.
   */
  def merge[TT](that: Target[TT]): Either[String, Target[T]] =
    if (this.value != that.value) {
      Left(s"Target.merge() called with two targets that don't have the same value: ${this.value} vs. ${that.value}")
    } else {
      val that2 = that.asInstanceOf[Target[T]]
      val deps  = (this.dependencies ++ that2.dependencies).distinct
      val pres  = this.preActions ++ that2.preActions
      val acts  = this.actions ++ that2.actions
      val posts = that2.postActions ++ this.postActions
      new Right(Target[T](this.name, this.value, deps, acts, pres, posts))
    }
}

/**
 * Encapsulates a Vector of targets, purely so that user can add an action literal
 * to all of them at once.
 */
case class TargetVector[T](targets: Vector[Target[T]]) {
  def apply(action: Context[T] => Result): TargetVector[T] =
    copy(targets = targets.map(t => t.copy(actions = t.actions :+ action)))
}
