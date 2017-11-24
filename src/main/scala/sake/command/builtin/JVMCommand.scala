package sake.command.builtin

import sake.command.ShellCommand
import sake.context.{Properties, Settings}
import sake.files.{File, Path}

class JVMCommand(name: String, options: Option[Map[Symbol,Any]]) extends ShellCommand(name, options) {

  def this(name:String, defaultOpts: Map[Symbol,Any]) = this(name, Some(defaultOpts))

  def this(name:String, defaultOpt0: (Symbol,Any), defaultOpts: (Symbol,Any)*) =
    this(name, Map[Symbol,Any](defaultOpt0) ++ defaultOpts)

  def this(name:String) = this(name, None)

  private def optionProcessor(key: Symbol, value: Any): Option[Seq[String]] =
    key match {
      case 'class     => Some(List(stringize(value)))
      case 'cp        => Some("-classpath" +: pathToString(value) +: Nil)
      case 'classpath => Some("-classpath" +: pathToString(value) +: Nil)
      case _          => None
    }

  optionsProcessor.addProcessor(optionProcessor _)

  // Make sure the classpath is set.
  override protected def optionsPostFilter(options: Map[Symbol,Any]) = {
    val (key, cp) = if (options.contains('cp))
      ('cp, options.getOrElse('cp, Path.empty[File]))
    else if (options.contains('classpath))
      ('classpath, options.getOrElse('classpath, Path.empty[File]))
    else
      ('cp, Path.empty[File])
    val cpath = cp match {
      case p: Path[_] => p
      case s: String => Path.make[File](s.split(Properties.pathSeparator), Properties.pathSeparator)(s => File(s))
    }
    super.optionsPostFilter(options.updated(key, cpath ++ Properties.classpath))
  }
}

