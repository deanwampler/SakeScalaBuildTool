package sake.util

/**
 * A simple logging facility.
 */
class Log(var threshold: Log.Level.Value = Log.Level.Info,
  var out: java.io.PrintStream = Console.out,
  var messageFormatter: (Log.Level.Value, String) => String = (l,s) => String.format("%s: %s",l,s)) {

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

  var default = new Log()

  def info(message: String): Unit   = default.info(message)
  def notice(message: String): Unit = default.notice(message)
  def warn(message: String): Unit   = default.warn(message)
  def error(message: String): Unit  = default.error(message)
  def fatal(message: String): Unit  = default.fatal(message)
}
