package sake.util

object Exit {

  def success(message: String = "") = {
    val sep = if (message.length > 0) ":" else "."
    Log.info(s"Exiting${sep} ${message}")
    sys.exit(0)
  }

  def fatal(message:String) = {
    Log.fatal(message)
    throw new BuildError(message)
  }

  def fatal(message:String, throwable: Throwable) = {
    Log.fatal(message)
    throw new BuildError(message, throwable)
  }
}

