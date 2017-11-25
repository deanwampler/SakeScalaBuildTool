package sake.command.builtin

import sake.command.{Command, Result, Passed, Failed}
import sake.files.{File, Path}

/**
 * Define and run an arbitrary Java program.
 * TODO: Currently constructs a command-line for the java program and runs it. Instead it should run in the same process.
 * TODO: Add Java 9 options. See http://www.oracle.com/technetwork/java/javase/documentation/index.html for more details.
 */
case object JavaCommand extends Command {

  import Argument._

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

  val versionRequired = Argument.nonemptyString(
    required = false,
    default  = None,
    description = "Warning: this feature is deprecated and will be removed in a future release. Require the specified version to run.")

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
    verbose,
    version,
    showVersion,
    versionRequired,
    jreRestrictSearch,
    jreNoRestrictSearch,
    enableAssertions,
    disableAssertions,
    enableSystemAssertions,
    disableSystemAssertions,
    classpath,
    systemProp,
    agentLib,
    agentPath,
    javaagent,
    splash)

  def java(args: ArgumentInstance[_]*): Result = run(args)
  def java(args: Seq[ArgumentInstance[_]]): Result = run(args)

  protected case class JavaTokens(
    javaOptions: Seq[String] = Vector.empty,
    classOrJarFile: Seq[String] = Vector.empty,
    appArgs: Seq[String] = Vector.empty) extends ShellCommandBase.TokensBase(Vector.empty, "", true) {

    def appendOptions(ops: String*) = copy(javaOptions = javaOptions ++ ops)
    def setClass(clazz: String) = copy(classOrJarFile = Seq(clazz))
    def setJarFile(jar: String) = copy(classOrJarFile = Seq("-jar", jar))
    def setAppArgs(args: Seq[String]) = copy(appArgs = args)

    def doValidate: (Boolean, String) =
      if (classOrJarFile.size == 0) (false, "Must specify either the class name or the jar file.")
      else (true, "")

    override def toSeq: Seq[String] =
      Vector("java") ++ javaOptions ++ classOrJarFile ++ appArgs
  }

  type Tokens = JavaTokens

  protected def defaultTokens: Tokens = JavaTokens()

  protected def handleArgument[V](tokens: Tokens, arg: Argument[V], value: V): Either[String, Tokens] = {

    def optString(s: Any): String =
      if (s.toString.trim.length > 0) ":"+s.toString.trim else ""
    def kvString(a: Any): String = {
      val kv = a.asInstanceOf[(String, Any)]
      s"${kv._1}=${kv._2}"
    }

    arg match {
      case `clazz` =>
        if (clazzOrJarGiven) Left(s"Can't specify both the class and app jar file: args = ${args.mkString(" ")}")
        else {
          clazzOrJarGiven = true
          Right(tokens.setClass(value.toString))
        }
      case `jar` =>
        if (clazzOrJarGiven) Left(s"Can't specify both the class and app jar file: args = ${args.mkString(" ")}")
        else {
          clazzOrJarGiven = true
          Right(tokens.setJarFile(value.toString))
        }
      case `appArgs`                   => Right(tokens.setAppArgs(value.asInstanceOf[Seq[String]]))
      case `help`                      => Right(tokens.appendOptions("-help"))
      case `helpX`                     => Right(tokens.appendOptions("-X"))
      case `d32`                       => Right(tokens.appendOptions("-d32"))
      case `d64`                       => Right(tokens.appendOptions("-d64"))
      case `server`                    => Right(tokens.appendOptions("-server"))
      case `verbose`                   => Right(tokens.appendOptions(s"-verbose${optString(value)}"))
      case `showVersion`               => Right(tokens.appendOptions("-showversion"))
      case `versionRequired`           => Right(tokens.appendOptions(s"-version${optString(value)}"))
      case `jreRestrictSearch`         => Right(tokens.appendOptions("-jre-restrict-search"))
      case `jreNoRestrictSearch`       => Right(tokens.appendOptions("-no-jre-restrict-search"))
      case `enableAssertions`          => Right(tokens.appendOptions(s"-enableassertions${optString(value)}"))
      case `disableAssertions`         => Right(tokens.appendOptions(s"-disableassertions${optString(value)}"))
      case `enableSystemAssertions`    => Right(tokens.appendOptions("-enablesystemassertions"))
      case `disableSystemAssertions`   => Right(tokens.appendOptions("-disablesystemassertions"))
      case `classpath`                 => Right(tokens.appendOptions("-classpath", value.toString))
      case `systemProp`                => Right(tokens.appendOptions(s"-D${kvString(value)}"))
      case `agentLib`                  => Right(tokens.appendOptions(s"-agentlib:${value}"))
      case `agentPath`                 => Right(tokens.appendOptions(s"-agentpath:${value}"))
      case `javaagent`                 => Right(tokens.appendOptions(s"-javaagent:${value}"))
      case `splash`                    => Right(tokens.appendOptions(s"-splash:${value}"))
      case unknown                     => Left(s"Unknown argument instance: argument = $argument, value = $value (all args: ${args.mkString(", ")}")
    }
  }
}
