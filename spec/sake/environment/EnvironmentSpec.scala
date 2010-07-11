package sake.environment

import sake.util.Path
import org.specs._

object EnvironmentSpec extends Specification { 

    var cpflag:Boolean = _
    
    doBeforeSpec {
        // Don't let CLASSPATH participate for most tests.
        cpflag = Environment.combineCLASSPATHandSystemClassPath
        Environment.combineCLASSPATHandSystemClassPath = false
    }
  
    doAfterSpec {
        Environment.combineCLASSPATHandSystemClassPath = cpflag
    }

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
            ((new Environment()).classpath.elements) must containAll (sysClassPath)
        }        
        
        "return a Path with the elements in the same order as the original path" in {
          ((new Environment()).classpath.elements) must containAll (sysClassPath)
        }        
        
        "be user changable, affecting the system path" in {
            val env = new Environment()
            env.classpath = "foo/bar" :: beforeCP
            env.classpath.elements must containAll ("foo/bar" :: beforeCP)
            beforeCP  must beDifferent (sysClassPath)
        }
        
        "remove duplicate entries, preserving priority order" in {
          val env = new Environment()
          env.classpath = sake.util.Path("foo/bar" :: "x/y" :: "x/y" :: "foo/bar" :: "baz/barf" :: Nil)(env.pathSeparator)
          env.classpath mustEqual (sake.util.Path("foo/bar" :: "x/y" :: "baz/barf" :: Nil)(env.pathSeparator))
        }
        
        "include the environment's CLASSPATH if Environment.combineCLASSPATHandSystemClassPath is true" in {
          Environment.combineCLASSPATHandSystemClassPath = true
          val env = new Environment()
          env.classpath must beDifferent(sysClassPath)
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
            val expected = Some(System.getenv.get("CLASSPATH"))
            Environment.getSystemEnvironmentVariables.get("CLASSPATH") mustEqual expected
        }
    }
}