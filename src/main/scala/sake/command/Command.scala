package sake.command

import sake.util._

/**
 * Abstractions for a command to run.
 * The user specified arguments will be checked to confirm that any required arguments are present.
 * Note that each argument can have a particular type, which will be checked at run time.
 */
trait Command {

  /** The name of the command. */
  val name: String

  /**
   * The set of supported arguments for this command, i.e., those known by the command type.
   */
  val supportedArguments: Set[Argument[_]]

  /** The set of required arguments. */
  lazy val requiredArguments: Set[Argument[_]] = supportedArguments.filter(_.required)

  /** The set of supported arguments with default values. */
  lazy val argumentsWithDefaults: Set[Argument[_]] = supportedArguments.filter(_.default != None)

  /**
   * The "action" that takes the supplied argument "instances" and performs the command's work.
   */
  val action: Seq[ArgumentInstance[_]] => Result

  /** Run the command by passing in a sequence of argument instances. */
  def run(arguments: ArgumentInstance[_]*): Result = run(arguments.toVector)

  /** Run the command by passing in a sequence of argument instances. */
  def run(arguments: Seq[ArgumentInstance[_]]): Result = try {
    def err(f: Failed): Failed  = { Log.error(s"Command $name failed: $f"); f }
    def info(p: Passed): Passed = { Log.info(s"Command $name successful! $p"); p }

    completeArguments(arguments) match {
      case Left(failed) => err(failed)
      case Right(args) => action(args) match {
        case f: Failed => err(f)
        case p: Passed => info(p)
      }
    }
  } catch {
    case scala.util.control.NonFatal(ex) => Failed.exception(s"command $name", ex)
  }

  protected def completeArguments(args: Seq[ArgumentInstance[_]]): Either[Failed, Seq[ArgumentInstance[_]]] = {
    val args2 = addDefaults(args.toVector)
    checkForMissingRequiredOptions(args2) match {
      case f: Failed => Left(f)
      case _ => Right(args2)
    }
  }

  // Combine the default arguments, then the user-specified arguments.
  protected def addDefaults(arguments: Vector[ArgumentInstance[_]]): Vector[ArgumentInstance[_]] = {
    val args = arguments.map(_.argument).toSet
    val missing = argumentsWithDefaults -- args
    arguments ++ (missing.map(arg => ArgumentInstance(arg, arg.default.get)))
  }

  protected def checkForMissingRequiredOptions(arguments: Vector[ArgumentInstance[_]]): Result = {
    val missingOptions = requiredArguments -- (arguments.map(_.argument).toSet)
    if (missingOptions.size > 0)
      Failed(1, s"Command $name requires these missing arguments: ${missingOptions.mkString(",")}")
    else
      Passed.default
  }

  /**
   * Definition of a command argument that combines a flag with an optional value of type `T`.
   * (If there is no value, we normally call those "flags".)
   * Note that by defining this class nested in the `Command` case class, we can
   * enforce that users can only pass arguments to commands that are defined _with_ the
   * the commands. For example, it will be a compile error to pass a `scalac` flag to a
   * `jar` command.
   * For the type, you'll typically use `String` or `Any` if you just want to pass
   * the argument through to the underlying command implementation. Consider using
   * Refined (https://github.com/fthomas/refined/) to further constrain the allowed values.
   * @param required  is the flag required? If so, the default value will be used, if defined and none is specified by the user.
   * @param default will be used if the argument is required, but not specified in a given command instance.
   * @param description for help messages.
   * @param validateErrorMessageFormat error message returned if `validate` fails. Used in printf-style message; it must contain one %s for the string version of the value.
   * @param validate a function to verify the value passed by a user is valid. Default is to accept as is.
   */
  case class Argument[-T](
    required: Boolean = false,
    default: Option[T] = None,
    description: String = "",
    validateErrorMessageFormat: String = """Value "%s" is not valid.""",
    validate: T => Boolean = (t: T) => true) {

    /** Validate a user-specified invocation of this argument. */
    def apply(value: T): Either[String, ArgumentInstance[T]] =
      if (validate(value)) Right(ArgumentInstance(this, value)) else Left(validateErrorMessageFormat.format(value.toString))
  }

  /**
   * When a build file uses an argument, this type encapsulates the argument
   * used and the value passed to it.
   */
  case class ArgumentInstance[-T](argument: Argument[T], value: T)

  object Argument {

    /**
     * Create an argument _instance_ for one or more "flags", i.e., a command-line
     * argument that doesn't have an associated value.
     * @param flags  a sequence of different flags allowed for the argument, e.g., "-h" and "--help". The command can interpret them as it sees fit.
     * @param required  does the user have to specify one of the flags?
     */
    def flag(description: String = ""): Argument[Unit] = Argument(description = description)

    /** Define a string argument with optional validation. */
    def string(
      required: Boolean = false,
      default: Option[String] = None,
      description: String = "",
      validate: String => Boolean = (s: String) => true): Argument[String] =
      Argument(
        required = required,
        default = default,
        description = description,
        validate = validate)

    /**
     * Define a string argument that can't be empty or contain just whitespace,
     * with optional additional validation.
     */
    def nonemptyString(
      required: Boolean = false,
      default: Option[String] = None,
      description: String = "",
      validateErrorMessageFormat: String = "",
      validate: String => Boolean = (s: String) => true): Argument[String] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = """String %s can not be empty. """ + validateErrorMessageFormat,
        (s: String) => s.trim.length > 0 && validate(s))


    /**
     * Define a key-value pair argument, with a string key and a user-specified
     * value type. They key can't be empty. The optional validation function,
     * can further constrain the key and value.
     * @param default note the type of the default is `(String,V)`, while the type parameter of the method is `V` and the returned Argument type is `(String,V)`.
     * @param validate note the type of validate is a two-argument function `(String,V) => Boolean`, while the type parameter of the method is `V` and the returned Argument type is `(String,V)`.
     */
    def keyValue[V](
      required: Boolean = false,
      default: Option[(String,V)] = None,
      description: String = "",
      validateErrorMessageFormat: String = "Key-value %s is invalid",
      validate: (String, V) => Boolean = (s: String, v: V) => true): Argument[(String,V)] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = validateErrorMessageFormat,
        (tup: (String,V)) => tup._1.trim.length > 0 && validate(tup._1, tup._2))


    import scala.math.Numeric
    import scala.math.Numeric._

    /** Define an argument where the value is a non-negative number, i.e., >= 0. */
    def nonnegative[T : Numeric](
      required: Boolean = false,
      default: Option[T] = None,
      description: String = ""): Argument[T] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "Value %s must be non-negative.",
        (n: T) => implicitly[Numeric[T]].compare(n, implicitly[Numeric[T]].zero) >= 0)

    /** Define an argument where the value is a positive number, i.e., > 0. */
    def positive[T : Numeric](
      required: Boolean = false,
      default: Option[T] = None,
      description: String = ""): Argument[T] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "Value %s must be positive.",
        (n: T) => implicitly[Numeric[T]].compare(n, implicitly[Numeric[T]].zero) > 0)

    /**
     * Define an argument for a sequence of items, e.g., for the string tokens to
     * construct a shell command, with optional validation of each element.
     * No validation of the sequence itself is done; the sequence can be empty, for example.
     */
    def seq[T](
      create: Boolean = false,
      required: Boolean = false,
      default: Option[Seq[T]] = None,
      description: String = "",
      validateErrorMessageFormat: String = "",
      validate: T => Boolean = (t: T) => true): Argument[Seq[T]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = validateErrorMessageFormat,
        (seq: Seq[T]) => seq.forall(validate))

    /**
     * Define an argument for a nonempty sequence of items, e.g., for the string tokens to
     * construct a shell command, with optional validation of each element.
     * No validation of the sequence itself is done other than ensuring the sequence isn't empty.
     */
    def nonemptySeq[T](
      create: Boolean = false,
      required: Boolean = false,
      default: Option[Seq[T]] = None,
      description: String = "",
      validateErrorMessageFormat: String = "",
      validate: T => Boolean = (t: T) => true): Argument[Seq[T]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "The sequence can't be empty. " + validateErrorMessageFormat,
        (seq: Seq[T]) => seq.size > 0 && seq.forall(validate))


    import sake.files.File

    /**
     * Define an argument where the value is a sequence of files that does not have to exist or you can
     * attempt to create it if it doesn't exist. Validation only fails if the create flag
     * is true and creation fails for any one of them.
     * This is useful, for example, for output files that don't yet exist.
     * @param create when one or more of the files doesn't exist. This requires the parent directory to exist. If the argument is true and creation fails, then it's treated as a validation error.
     */
    def files(
      create: Boolean = false,
      required: Boolean = false,
      default: Option[Seq[File]] = None,
      description: String = ""): Argument[Seq[File]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "At least one of %s is not a file that already exists and creating it failed.",
        (seq: Seq[File]) => filesValidate(create, seq))

    /**
     * Define an argument where the value is a file that does not have to exist or you can
     * attempt to create it if it doesn't exist. Validation only fails if the create flag
     * is true and creation fails.
     * This is useful, for example, for an output file that doesn't yet exist.
     * @param create if the file doesn't exist. This requires the parent directory to exist. If the argument is true and creation fails, then it's treated as a validation error.
     */
    def file(
      create: Boolean = false,
      required: Boolean = false,
      default: Option[File] = None,
      description: String = ""): Argument[File] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "%s is not a file that already exists and creating it failed.",
        (f: File) => filesValidate(create, Seq(f)))

    /**
     * Define an argument where the value is a sequence of directories that do not have to exist or you can
     * attempt to create them if they don't exist. Validation only fails if the create flag
     * is true and creation fails for any one of them. Note that parent directories will be created if necessary,
     * which is different than the behavior of {@link #files}.
     * @param create when one or more directories doesn't exist. This will create the parent directories, if necessary. If the argument is true and creation fails, then it's treated as a validation error.
     */
    def directories(
      create: Boolean = false,
      required: Boolean = false,
      default: Option[Seq[File]] = None,
      description: String = ""): Argument[Seq[File]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "At least one of %s is not a directory that already exists and creating it failed.",
        (seq: Seq[File]) => directoriesValidate(create, seq))

    /**
     * Define an argument where the value is a directory that does not have to exist or you can
     * attempt to create it if it doesn't exist. Validation only fails if the create flag
     * is true and creation fails. Note that parent directories will be created if necessary,
     * which is different than the behavior of {@link #file}.
     * @param create if the directory doesn't exist, then create it. This will create the parent directories, if necessary. If the argument is true and creation fails, then it's treated as a validation error.
     */
    def directory(
      create: Boolean = false,
      required: Boolean = false,
      default: Option[File] = None,
      description: String = ""): Argument[File] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "%s is not a directory that already exists and creating it failed.",
        (d: File) => directoriesValidate(create, Seq(d)))

    /**
     * Define an argument where the value is a sequence of files, not directories, that already exist.
     * If you want creation on the fly, use {@link #files}.
     */
    def existingFiles(
      required: Boolean = false,
      default: Option[Seq[File]] = None,
      description: String = ""): Argument[Seq[File]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "At least one of %s is not a file that already exists.",
        (seq: Seq[File]) => seq forall (_.isFile))

    /**
     * Define an argument where the value is a file, not a directory, that already exists.
     * If you want creation on the fly, use {@link #file}.
     */
    def existingFile(
      required: Boolean = false,
      default: Option[File] = None,
      description: String = ""): Argument[File] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "%s is not a file that already exists.",
        (f: File) => f.isFile)

    /**
     * Define an argument where the value is a sequence of directories, not files, that already exist.
     * If you want creation on the fly, use {@link #directories}.
     */
    def existingDirectories(
      required: Boolean = false,
      default: Option[Seq[File]] = None,
      description: String = ""): Argument[Seq[File]] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "At least one of %s is not a directory that already exists.",
        (seq: Seq[File]) => seq forall (_.isDirectory))

    /**
     * Define an argument where the value is a directory, not a file, that already exists.
     * If you want creation on the fly, use {@link #directory}.
     */
    def existingDirectory(
      required: Boolean = false,
      default: Option[File] = None,
      description: String = ""): Argument[File] =
      Argument(
        required = required,
        default = default,
        description = description,
        validateErrorMessageFormat = "%s is not a directory that already exists.",
        (d: File) => d.isDirectory)

    import sake.files.Path

    /** Define an argument where the value is a path. No validation is done. */
    def path[T](
      required: Boolean = false,
      default: Option[Path[T]] = None,
      description: String = ""): Argument[Path[T]] =
      Argument(
        required = required,
        default = default,
        description = description)

    protected def seqValidate(create: Boolean, seq: Seq[File])(exists: File => Boolean)(doCreate: File => Boolean): Boolean = {
      if (create == false) true // do nothing else
      else try {
        seq forall (f => exists(f) || doCreate(f))
      } catch {
        case _: java.io.IOException => false
      }
    }
    protected def filesValidate(create: Boolean, seq: Seq[File]): Boolean =
      seqValidate(create, seq)(f => f.isFile)(f => f.createNewFile)

    protected def directoriesValidate(create: Boolean, seq: Seq[File]): Boolean =
      seqValidate(create, seq)(f => f.isDirectory)(f => f.mkdirs)
  }
}
