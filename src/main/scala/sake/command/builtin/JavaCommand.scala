package sake.command.builtin

import sake.files.{File, Path}

/**
 * Define and run an arbitrary Java program.
 * TODO: Add Java 9 options. See http://www.oracle.com/technetwork/java/javase/documentation/index.html for more details.
 */
case object JavaCommand extends Command {

  import Command.Argument._

  val name = "JavaCommand"

  val clazz = Argument.nonemptyString(
    required = false,
    default  = None,
    description = "the main class name")
  val jar = Argument.file(
    required = false,
    default  = None,
    description = "the application jar file")
  val appArgs = Argument.seq[String](
    required = false,
    default  = None,
    description = "arguments for the application")

  val help   = Argument.flag("show the help message")
  val helpX  = Argument.flag("print help on non-standard options")

  val d32    = Argument.flag("use a 32-bit data model if available")
  val d64    = Argument.flag("use a 64-bit data model if available")
  val server = Argument.flag("""to select the "server" VM. The default VM is server, because you are running on a server-class machine.""")

  val version = Argument.flag("print the JVM version and exit")
  val showVersion = Argument.flag("print the JVM version and continue")

  val jreRestrictSearch = Argument.flag("Warning: this feature is deprecated and will be removed in a future release. Include user private JREs in the version search")
  val jreNoRestrictSearch = Argument.flag("Warning: this feature is deprecated and will be removed in a future release. Exclude user private JREs in the version search")

  val enableSystemAssertions = Argument.flag("enable system assertions")
  val disableSystemAssertions = Argument.flag("enable system assertions")

  val classpath = Argument.path(
    required = false,
    default  = None,
    description = "class search path of directories and zip/jar files. A colon separated list of directories, JAR archives, and ZIP archives to search for class files.")

  val systemProp = Argument.keyValue[String](
      required = false,
      default  = None,
      description = """The java "-D<name>=<value>" flag to set a system property""")

  val verbose = Argument.nonemptyString(
      required = false,
      default  = None,
      description = """enable verbose output. The value must be one of "class", "gc", or "jni" """,
      validateErrorMessageFormat = """The verbose argument %s is not valid. It must be one of "class", "gc", or "jni" """,
      validate = s => s == "class" || s == "gc" || s == "jni")

  val versionRequired = Argument.nonemptyString(
      required = false,
      default  = None,
      description = "Warning: this feature is deprecated and will be removed in a future release. Require the specified version to run.")

  val enableAssertions = Argument.string(
      required = false,
      default  = None,
      description = """enable assertions with the specified granularity.
        |The value must be empty or one of "packagename", "packagename...", "...", or "classname"
        |for a particular package or class. The "..." by itself means the package corresponding
        |to the current directory and all subpackages, while "packagename..." means the named
        |package and all subpackages.""")

  val disableAssertions = Argument.string(
      required = false,
      default  = None,
      description = """disable assertions with the specified granularity.
        |The value must be empty or one of "packagename", "packagename...", "...", or "classname"
        |for a particular package or class. The "..." by itself means the package corresponding
        |to the current directory and all subpackages, while "packagename..." means the named
        |package and all subpackages.""".stripMargin)

  val agentLib = Argument.nonemptyString(
      required = false,
      default  = None,
      description = """load a native agent library, e.g. "hprof".
        |The argument must be of the form "libname[=<options>]".
        |See also, java -agentlib:jdwp=help and java -agentlib:hprof=help""".stripMargin)

  val agentPath = Argument.nonemptyString(
      required = false,
      default  = None,
      description = "load native agent library by full pathname. pathname[=<options>]")

  val javaagent = Argument.nonemptyString(
      required = false,
      default  = None,
      description = "load Java programming language agent, see java.lang.instrument. jarpath[=<options>]")

  val splash = Argument.nonemptyString(
      required = false,
      default  = None,
      description = "show splash screen with specified image. The argument is the image path")

  val supportedArguments: Set[Argument[_]] = Set(
    clazz,
    jar,
    appArgs,
    help,
    helpX,
    d32,
    d64,
    server,
    version,
    showVersion,
    jreRestrictSearch,
    jreNoRestrictSearch,
    enableSystemAssertions,
    disableSystemAssertions,
    classpath,
    systemProp,
    verbose,
    versionRequired,
    enableAssertions,
    disableAssertions,
    agentLib,
    agentPath,
    javaagent,
    splash)

  def sh(command: String): Result = commandStringArg(command) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  def sh(commandTokens: Seq[String]): Result = commandTokensArg(commandTokens) match {
    case Left(error) => Failed(1, error)
    case Right(argInstance) => run(argInstance)
  }

  import scala.sys.process._

  val action: Seq[ArgumentInstance[_]] => Result = {
    case ArgumentInstance(commandStringArg, commandString: String) +: Nil =>
      val exitCode = commandString.!
      if (exitCode == 0) Passed.default
      else Failed(exitCode, "Java command failed: $commandString")
    case ArgumentInstance(commandTokensArg, commandTokens: Seq[_]) +: Nil =>
      val tokens = commandTokens.map(_.toString)      // force "_" to be String
      val exitCode = tokens.!
      if (exitCode == 0) Passed.default
      else Failed(exitCode, s"Java command failed: ${tokens.mkString(" ")}")
    case Nil =>
      Failed(1, "No command string or tokens specified!")
    case seq =>
      Failed(1, s"Can't specify the command using both a string and a sequence of tokens: ${seq.mkString(", ")}")
  }
}
