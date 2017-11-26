package sake

// import scala.collection.immutable._
import sake.command.builtin.Commands
import sake.command.{Result, Passed, Failed}
import sake.context.{Context, Settings}
import sake.target.{Target, TargetVector}
import sake.util.{Exit, Log}
import sake.files.{File, FilesFinder, JavaFilesFinder}

/**
* The Project class defines actual build scripts.
*/
class Project(val name: String) extends Commands {

  Project.add(this)

  var settings: Settings = Settings.default
  var log: Log = Log.default
  var logLevel: Log.Level.Value = Log.Level.Info

  var showStackTracesOnFailures = true

  /** The user asks to build targets. Verify they exist and build them in order! */
  def build(targetNames: String*): Result = {
    def b(lastResult: Result, targs: Seq[Target[_]]): Result = targs match {
      case Nil => lastResult
      case head +: tail =>
        val context = Context(head, settings, Map.empty[String, Any])  // TODO what should go in the map?
        val result = doBuild(context, head)
        if (result.passed) b(result, tail) else result
    }
    determineTargets(targetNames) match {
      case Right(targets) => b(Passed.default, targets) match {
        case p: Passed => reportSuccess(p); p
        case f: Failed => reportBuildError(f); f
      }
      case Left(error) =>
        val f = Failed(1, error)
        reportBuildError(f)
        f
    }
  }

  import scala.language.implicitConversions

  /**
   * An implicit conversion that allows you to use the Scala process API, where
   * integer exit codes are returned, as actions for targets. This function lifts
   * a function of type `Context => Int` to `Context => Result`,
   * based on the exit code returned.
   */
  implicit def liftExitCode(f: Context => Int): Context => Result =
    (c: Context) => f(c) match {
      case 0 => Passed.default
      case n => Failed(n, s"Command failed, returned exit code $n")
    }

  protected def determineTargets(targetNames: Seq[String]): Either[String, Seq[Target[_]]] = {
    def buildOrder(targetNames: Seq[Target[_]]): Seq[Target[_]] = {
      targetNames.foldLeft(Vector.empty[Target[_]]) { (allTargs, t) =>
        val target = Target.targets(t.name) // we already know it's there, so no need to verify.
        // It might seem unsafe to call buildOrder and not determineTargets; how
        // do we know the dependencies exist? They do, because they are defined
        // already as targets. determineTargets is called when the user invokes
        // the build, so it catches user errors at build time.
        // Put dependencies before the target!
        allTargs ++ buildOrder(target.dependencies) :+ target
      }
    }

    val (targets, unknowns) = targetNames.foldLeft(Vector.empty[Target[_]], Vector.empty[String]) {
      case ((targets, unknowns), name) => Target.targets.get(name) match {
        case None => (targets, unknowns :+ name)
        case Some(target) => (targets :+ target, unknowns)
      }
    }
    unknowns.size match {
      case 0 => Right(buildOrder(targets).distinct)
      case 1 => Left(s"Unknown target: ${unknowns.head}")
      case _ => Left(s"Unknown targets: ${unknowns.mkString(", ")}")
    }
  }

  protected def doBuild(context: Context, target: Target[_]): Result = {
    log(logLevel, "building "+target.value)
    try {
      target.build(context)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${target.name}", ex)
    }
  }

  /** Force an action to succeed */
  def success: Result = Passed.default

  // Define methods the user would typically use in a build file to create targets.
  // The assumption is that the actions wouldn't be passed as arguments, as in the
  // Target constructor, but instead would be defined using the Target#apply method.

  /**
   * Create a target with no dependencies (they could be added later).
   * Example: target(name) { action }
   * @return the new Target. Also updates the internal map of targets.
   */
  def target[T](value: T): Target[T] =
    Target[T](value, dependencies = Vector.empty, actions = Vector.empty)

  /**
   * Create a target with a sequence of dependencies.
   * Example: target(name, dependencies = Seq(dep1, dep2, ...)) { action }
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](value: T, dependencies: Seq[Target[_]]): Target[T] =
    Target[T](value, dependencies, actions = Vector.empty)

  /**
   * Create a target with a sequence of dependencies, using a tuple syntax.
   * Example: target("name" -> Seq("a", "b")) { action }
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](value_dependencies: (T, Seq[Target[_]])): Target[T] =
    Target[T](value_dependencies, actions = Vector.empty)

  /**
   * Create one or more targets, passed in as a sequence, with no dependencies.
   * If present, the action literal will apply to all the targets.
   * Example: target(names = Seq(name1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   */
  def target[T](values: Seq[T]): TargetVector[T] =
    Target[T](values, dependencies = Vector.empty, actions = Vector.empty)

  /**
   * Create one or more targets, passed in as a sequence, with the same sequence
   * of dependencies. If present, the action literal will apply to all the targets.
   * Example: target(names = Seq(name1, ...), dependencies = Seq(dep1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](values: Seq[T], dependencies: Seq[Target[_]]): TargetVector[T] =
    Target[T](values, dependencies, actions = Vector.empty)

  /**
   * Create one or more targets, passed in as a sequence, with a sequence of
   * dependencies, which will apply to each target. If present, the action literal
   * will apply to all the targets.
   * Example: target(names_dependencies = (Seq(name1, ...) -> Seq(dep1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](values_dependencies: (Seq[T], Seq[Target[_]])): TargetVector[T] =
    Target[T](values_dependencies, actions = Vector.empty)


  /** Return the files in a directory. */
  def findInDir(parent: File): Seq[File] =
    JavaFilesFinder.findInDir(parent)

  /** Return the files in a directory that match a glob pattern. */
  def findInDir(parent: File, filePattern: String): Seq[File] =
    JavaFilesFinder.findInDir(parent, filePattern)

  /** Return the files in one or more directories. */
  def findInDir(parents: Seq[File]): Seq[File] =
    JavaFilesFinder.findInDir(parents)

  /** Return the files in one or more directories that match a glob pattern. */
  def findInDir(parents: Seq[File], filePattern: String): Seq[File] =
    JavaFilesFinder.findInDir(parents, filePattern)

  /** Return the files in a directory, recursively. */
  def findInDirRecursive(parent: File): Seq[File] =
    JavaFilesFinder.findInDirRecursive(parent)

  /** Return the files in a directory, recursively, that match a glob pattern. */
  def findInDirRecursive(parent: File, filePattern: String): Seq[File] =
    JavaFilesFinder.findInDirRecursive(parent, filePattern)

  /** Return the files in one or more directories, recursively. */
  def findInDirRecursive(parents: Seq[File]): Seq[File] =
    JavaFilesFinder.findInDirRecursive(parents)

  /** Return the files in one or more directories, recursively, that match a glob pattern. */
  def findInDirRecursive(parents: Seq[File], filePattern: String): Seq[File] =
    JavaFilesFinder.findInDirRecursive(parents, filePattern)

  /**
   * Ensure that a directory exists, with no dependencies, but fail if the parent
   * doesn't exist or the creation fails.
   * Example: mkdir(File(dirName))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdir(directory: File): Target[File] =
    Target(directory, dependencies = Vector.empty, mkdirsAction(Seq(directory), false))

  /**
   * Ensure that a directory exists, after satisfying the dependencies, but fail
   * if the parent doesn't exist or the creation fails.
   * Example: mkdir(File(dirName), dependencies = Seq(dep1, dep2, ...))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdir(directory: File, dependencies: Seq[Target[_]]): Target[File] =
    Target(directory, dependencies, mkdirsAction(Seq(directory), false))

  /**
   * Ensure that a directory exists, after satisfying the dependencies, but fail
   * if the parent doesn't exist or the creation fails.
   * Example: mkdir(File(dirName) -> Seq(dep1, dep2, ...))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdir(directory_dependencies: (File, Seq[Target[_]])): Target[File] =
    Target(directory_dependencies, mkdirsAction(Seq(directory_dependencies._1), false))

  /**
   * Ensure that one or more directories exist, with no dependencies, but fail
   * if a parent doesn't exist, or if a creation fails. The process stops on the first failure.
   * Example: mkdir(Seq(File(dirName1), File(dirName2)))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdir(directories: Seq[File]): Target[Seq[File]] =
    Target(directories, dependencies = Vector.empty, mkdirsAction(directories, false))

  /**
   * Ensure that one or more directories exist, after satisfying the dependencies, but fail
   * if a parent doesn't exist, or if a creation fails. The process stops on the first failure.
   * Example: mkdir(Seq(File(dirName1), File(dirName2)), Seq(dep1, dep2, ...))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdir(directories: Seq[File], dependencies: Seq[Target[_]]): Target[Seq[File]] =
    Target(directories, dependencies, mkdirsAction(directories, false))


  /**
   * Ensure that a directory exists, with no dependencies. Create the parents if
   * they don't exist. Fail if any creation step fails.
   * Example: mkdirs(File(dirName))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdirs(directory: File): Target[File] =
    Target(directory, dependencies = Vector.empty, mkdirsAction(Seq(directory), true))

  /**
   * Ensure that a directory exists, after satisfying the dependencies. Create the parents if
   * they don't exist. Fail if any creation step fails.
   * Example: mkdirs(File(dirName), dependencies = Seq(dep1, dep2, ...))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdirs(directory: File, dependencies: Seq[Target[_]]): Target[File] =
    Target(directory, dependencies, mkdirsAction(Seq(directory), true))

  /**
   * Ensure that a directory exists, after satisfying the dependencies. Create the parents if
   * they don't exist. Fail if any creation step fails.
   * Example: mkdirs(File(dirName) -> Seq(dep1, dep2, ...))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdirs(directory_dependencies: (File, Seq[Target[_]])): Target[File] =
    Target(directory_dependencies, mkdirsAction(Seq(directory_dependencies._1), true))

  /**
   * Ensure that one or more directories exist, with no dependencies. Create the parents if
   * they don't exist. Fail if any creation step fails. The process stops on the first failure.
   * Example: mkdirs(Seq(File(dirName1), File(dirName2)))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdirs(directories: Seq[File]): Target[Seq[File]] =
    Target(directories, dependencies = Vector.empty, mkdirsAction(directories, true))

  /**
   * Ensure that one or more directories exist, after satisfying the dependencies. Create the parents if
   * they don't exist. Fail if any creation step fails. The process stops on the first failure.
   * Example: mkdirs(Seq(File(dirName1), File(dirName2)), Seq(dep1, dep2, ...))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def mkdirs(directories: Seq[File], dependencies: Seq[Target[_]]): Target[Seq[File]] =
    Target(directories, dependencies, mkdirsAction(directories, true))


  /**
   * Create a file, with no dependencies.
   * Example: file(afile) { action }
   * @return  the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(file: File): Target[File] =
    Target(file, dependencies = Vector.empty, actions = Vector.empty)

  /**
   * Create a file, based on a sequence of dependencies.
   * Example: file(afile, dependencies = Seq(dep1, dep2, ...)) { action }
   * @return  the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(file: File, dependencies: Seq[Target[_]]): Target[File] =
    Target(file, dependencies, actions = Vector.empty)

  /**
   * Create a file, based on a sequence of dependencies and actions.
   * Example: file(afile -> Seq("a", "b")) { action }
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(file_dependencies: (File, Seq[Target[_]])): Target[File] =
    Target(file_dependencies, actions = Vector.empty)

  /**
   * Create one or more files, passed in as a sequence, with no dependencies.
   * If an action literal is present, it is applied to all of them.
   * Example: file(Seq(file1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files: Seq[File]): TargetVector[File] =
    Target(files, dependencies = Vector.empty, actions = Vector.empty)

  /**
   * Create one or more files, passed in as a sequence, with the same sequence of
   * dependencies. If an action literal is present, it is applied to all of them.
   * Example: file(Seq(file1, ...), dependencies = Seq(dep1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files: Seq[File], dependencies: Seq[Target[_]]): TargetVector[File] =
    Target(files, dependencies, actions = Vector.empty)

  /**
   * Create one or more files, passed in as a sequence, with the shared sequence of
   * dependencies. If an action literal is present, it is applied to all of them.
   * Example: file((Seq(file1, ...) -> Seq(dep1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files_dependencies: (Seq[File], Seq[Target[_]])): TargetVector[File] =
    Target(files_dependencies, actions = Vector.empty)

  /**
   * Delete the target, which has no dependencies. For a directory, it deletes
   * recursively.
   * Example: clean(File(dirName))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   */
  def clean(directory: File): Target[File] =
    Target[File](directory, Vector.empty, cleanAction(Seq(directory)))

  /**
   * Delete the target, after satisfying the dependencies. For a directory, it deletes
   * recursively.
   * Example: clean(File(dirName), dependencies = Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directory: File, dependencies: Seq[Target[_]]): Target[File] =
    Target[File](directory, dependencies, cleanAction(Seq(directory)))

  /**
   * Delete the target, after satisfying the dependencies. For a directory, it deletes
   * recursively.
   * Example: clean(File(dirName) -> Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directory_dependencies: (File, Seq[Target[_]])): Target[File] =
    Target[File](directory_dependencies, cleanAction(Seq(directory_dependencies._1)))

  /**
   * Delete one or more targets, which have no dependencies. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(Seq(File(dirName1), File(dirName2)))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   */
  def clean(directories: Seq[File]): Target[Seq[File]] =
    Target[Seq[File]](directories, Vector.empty, cleanAction(directories))

  /**
   * Delete one or more targets, after satisfying the dependencies. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(Seq(File(dirName1), File(dirName2)), Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directories: Seq[File], dependencies: Seq[Target[_]]): Target[Seq[File]] =
    Target[Seq[File]](directories, dependencies, cleanAction(directories))


  protected def mkdirsAction(files: Seq[File], createParents: Boolean): Vector[Context => Result] = {
    val md = if (createParents) (f: File) => f.mkdirs else (f: File) => f.mkdir
    def mds(fs: Seq[File], r: Result): Result = fs match {
      case Nil => r
      case head +: tail =>
        if (md(head)) mds(tail, r)
        else Failed(1, s"Failed to create directory $head (create parents? $createParents)")
    }

    Vector((c:Context) => mds(files, Passed.default))
  }

  protected def cleanAction(files: Seq[File]): Vector[Context => Result] = {
    def cs(fs: Seq[File], r: Result): Result = fs match {
      case Nil => r
      case head +: tail =>
        if (head.deleteRecursively) cs(tail, r)
        else Failed(1, s"Failed to delete $head")
    }

    Vector((c:Context) => cs(files, Passed.default))
  }

  private def reportSuccess(passed: Passed): Unit =
    Log.info(s"Success! $passed")

  private def reportBuildError(failed: Failed): Unit = {
    Log.error(s"Build failure: $failed")
    failed.cause match {
      case Some(cause) if showStackTracesOnFailures => cause.printStackTrace(log.out)
      case _ => // do nothing
    }
  }
}

object Project {
  var all: Map[String, Project] = Map.empty

  def add(p: Project): Unit =
    if (all.contains(p.name)) {
      Exit.fatal(s"Projects must have unique names. This one: ${p.name}. Defined projects: ${all.keySet.mkString(", ")}")
    } else {
      all += (p.name -> p)
    }
}
