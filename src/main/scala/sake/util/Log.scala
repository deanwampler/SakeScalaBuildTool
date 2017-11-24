package sake.util

/**
 * A simple logging facility.
 */
class Log(var threshold: Log.Level.Value,
  var out: java.io.PrintStream,
  var messageFormatter: (Log.Level.Value, String) => String) {

  def this(threshold: Log.Level.Value, out: java.io.PrintStream) =
      this(threshold, out, (l,s) => String.format("%s: %s",l,s))
  def this(threshold: Log.Level.Value) = this(threshold, Console.out)
  def this() = this(Log.Level.Warn)

  def info(message: String): Unit   = apply(Log.Level.Info,   message)
  def notice(message: String): Unit = apply(Log.Level.Notice, message)
  def warn(message: String): Unit   = apply(Log.Level.Warn,   message)
  def error(message: String): Unit  = apply(Log.Level.Error,  message)
  def fatal(message: String): Unit  = apply(Log.Level.Fatal,  message)

  def apply(level: Log.Level.Value, message: String):Unit =
      if (level >= threshold) out.println(messageFormatter(level, message))
}

object Log {
  object Level extends Enumeration {
    val Info   = Value("Info")
    val Notice = Value("Notice")
    val Warn   = Value("Warn")
    val Error  = Value("Error")
    val Fatal  = Value("Fatal")
  }

  var log = new Log()
}
