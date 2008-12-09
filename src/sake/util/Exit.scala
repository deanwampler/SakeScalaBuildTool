package sake.util

object Exit {
    /**
     * Error out of the build.
     */
    def error(message:String) = throw new BuildError(message)
    def error(message:String, throwable: Throwable) = throw new BuildError(message, throwable)
}
