package sake.environment

import org.specs._

object EnvironmentSpec extends Specification { 

    def sysClassPath = {
        val sep = System.getProperty("path.separator")
        val seq = for {
            s <- System.getProperty("java.class.path").split(sep)
        } yield s
        sake.util.Path(seq.foldLeft[List[String]](Nil) {(cp, elem) => elem :: cp }.reverse)(sep)
    }
    
    "The classpath" should {
        val beforeCP = sysClassPath
        
        doAfter {
            Environment.environment.classpath = beforeCP  // reset
        }
        
        "return the system classpath converted to a Path" in {
            new Environment().classpath mustEqual sysClassPath
        }        
        
        "return a Path with the elements in the same order as the original path" in {
            new Environment().classpath mustEqual sysClassPath
        }        
        
        "be user changable, affecting the system path" in {
            val env = new Environment()
            env.classpath = "foo/bar" :: beforeCP
            env.classpath mustEqual ("foo/bar" :: beforeCP)
            sysClassPath  must beDifferent(beforeCP)
            sysClassPath  mustEqual env.classpath
        }
    }
    
    "The pathSeparator" should {
        "be the platform's path separator" in {
            new Environment().pathSeparator mustEqual System.getProperty("path.separator")
        }
    }
    
    "The fileSeparator" should {
        "be the platform's file separator" in {
            new Environment().fileSeparator mustEqual System.getProperty("file.separator")
        }
    }
    
    "The lineSeparator" should {
        "be the platform's line separator" in {
            new Environment().lineSeparator mustEqual System.getProperty("line.separator")
        }
    }
    
    "The currentWorkingDirectory" should {
        "be the user's current directory" in {
            new Environment().currentWorkingDirectory mustEqual System.getProperty("user.dir")
        }
    }
    
    "The System Property getter" should {
        "return the system property" in {
            val expected = System.getProperty("java.class.path")
            Environment.getSystemProperty("java.class.path") mustEqual expected
        }
    }
    
    "The System Property setter" should {
        "set the system property" in {
            val save = System.getProperty("java.class.path")
            val expected = save + System.getProperty("file.separator") + "/foo/bar"
            Environment.setSystemProperty("java.class.path", expected)
            Environment.getSystemProperty("java.class.path") mustEqual expected
            Environment.setSystemProperty("java.class.path", save)
        }
    }

    "The System Environment Variables getter" should {
        "return the system environment, a map of defined variables" in {
            val expected = System.getenv.get("CLASSPATH")
            Environment.getSystemEnvironmentVariables.get("CLASSPATH") mustEqual expected
        }
    }
}