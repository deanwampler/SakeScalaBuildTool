package sake.environment

import org.specs._

object EnvironmentSpec extends Specification { 

    def sysClassPath = {
        val seq = for {
            s <- System.getProperty("java.class.path").split(System.getProperty("path.separator"))
        } yield s
        seq.foldLeft[List[String]](Nil) {(cp, elem) => elem :: cp }
    }
    
    "The classpath" should {
        
        "default to the system classpath converted to a List" in {
            new Environment().classpath must be_==(sysClassPath)
        }        
        
        "be user changable, without affecting the system path. to the system classpath converted to a List" in {
            val beforeCP = sysClassPath
            val env = new Environment()
            env.classpath = "foo/bar" :: beforeCP
            env.classpath must be_==("foo/bar" :: beforeCP)
            sysClassPath must be_==(beforeCP)
        }
    }
    
    "The pathSeparator" should {
        "be the platform's path separator" in {
            new Environment().pathSeparator must be_==(System.getProperty("path.separator"))
        }
    }
    
    "The fileSeparator" should {
        "be the platform's file separator" in {
            new Environment().fileSeparator must be_==(System.getProperty("file.separator"))
        }
    }
    
    "The currentWorkingDirectory" should {
        "be the user's current directory" in {
            new Environment().currentWorkingDirectory must be_==(System.getProperty("user.dir"))
        }
    }
    
    "The systemProperty getter" should {
        "return the system property" in {
            val expected = System.getProperty("java.class.path")
            Environment.systemProperty("java.class.path") must be_==(expected)
        }
    }
    
    "The systemProperty setter" should {
        "set the system property" in {
            val save = System.getProperty("java.class.path")
            val expected = save + System.getProperty("file.separator") + "/foo/bar"
            Environment.systemProperty("java.class.path", expected)
            Environment.systemProperty("java.class.path") must be_==(expected)
            Environment.systemProperty("java.class.path", save)
        }
    }
}