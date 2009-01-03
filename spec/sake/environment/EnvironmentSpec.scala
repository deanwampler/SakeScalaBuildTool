package sake.environment

import org.specs._

object EnvironmentSpec extends Specification { 

    def sysClassPath = {
        val seq = for {
            s <- System.getProperty("java.class.path").split(System.getProperty("path.separator"))
        } yield s
        seq.foldLeft[List[String]](Nil) {(cp, elem) => elem :: cp }.reverse
    }
    
    "The classpath" should {
        val beforeCP = sysClassPath
        
        doAfter {
            Environment.environment.classpath = beforeCP  // reset
        }
        
        "return to the system classpath converted to a List" in {
            new Environment().classpath mustEqual sysClassPath
        }        
        
        "return to list with the elements in the same order as the original path" in {
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
    
    "The systemProperty getter" should {
        "return the system property" in {
            val expected = System.getProperty("java.class.path")
            Environment.getSystemProperty("java.class.path") mustEqual expected
        }
    }
    
    "The systemProperty setter" should {
        "set the system property" in {
            val save = System.getProperty("java.class.path")
            val expected = save + System.getProperty("file.separator") + "/foo/bar"
            Environment.setSystemProperty("java.class.path", expected)
            Environment.getSystemProperty("java.class.path") mustEqual expected
            Environment.setSystemProperty("java.class.path", save)
        }
    }
}