package sake.util

object Level extends Enumeration { 
    val Info = Value("Info") 
    val Notice = Value("Notice")
    val Warn = Value("Warn")
    val Error = Value("Error")
    val Fatal = Value("Fatal")
}

/** 
 * A simple logging facility.
 */
class Log(var threshold: Level.Value, 
    var out: java.io.PrintStream, 
    var messageFormatter: (Level.Value, String) => String) {

    def this(threshold: Level.Value, out: java.io.PrintStream) = 
        this(threshold, out, (l,s) => String.format("%s: %s",l,s))
    def this(threshold: Level.Value) = this(threshold, Console.out)
    def this() = this(Level.Warn)
    
    def apply(level: Level.Value, message: String):Unit = 
        if (level >= threshold) out.println(messageFormatter(level, message))
}

object Log {

    var log = new Log()
}