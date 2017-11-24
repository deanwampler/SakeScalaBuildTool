package sake.util

object Exit {

    def success(message: String = "") = {
      val sep = if (message.length > 0) ":" else "."
      Log.log.info(s"Exiting${sep} ${message}")
      sys.exit(0)
    }

    def error(message:String) = {
        logMessage(message)
        throw new BuildError(message)
    }

    def error(message:String, throwable: Throwable) = {
        logMessage(message)
        throw new BuildError(message, throwable)
    }

    private def logMessage(message: String) = Log.log.fatal(message)
}

