package sake.util

import sake.environment.Environment

object Exit {
    
    def error(message:String) = {
        logMessage(message)
        throw new BuildError(message)
    }

    def error(message:String, throwable: Throwable) = {
        logMessage(message)
        throw new BuildError(message, throwable)
    }
    
    private def logMessage(message: String) = Log.log(Level.Fatal, message)
}
    
