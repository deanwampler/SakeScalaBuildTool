package sake.context

import sake.files.{File, Path}
import org.scalatest._
import org.scalatest.Matchers._

object EnvironmentSpec extends FreeSpec {

    var cpflag:Boolean = _

    doBeforeSpec {
        // Don't let CLASSPATH participate for most tests.
        cpflag = Environment.combineCLASSPATHandSystemCLASSPATH
        Environment.combineCLASSPATHandSystemCLASSPATH = false
    }

    doAfterSpec {
        Environment.combineCLASSPATHandSystemCLASSPATH = cpflag
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
            Environment.classpath = beforeCP  // reset
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
          env.classpath shouldEqual (sake.util.Path("foo/bar" :: "x/y" :: "baz/barf" :: Nil)(env.pathSeparator))
        }

        "include the environment's CLASSPATH if Environment.combineCLASSPATHandSystemCLASSPATH is true" in {
          Environment.combineCLASSPATHandSystemCLASSPATH = true
          val env = new Environment()
          env.classpath must beDifferent(sysClassPath)
        }
    }

    "The pathSeparator" should {
        "be the platform's path separator" in {
            new Environment().pathSeparator shouldEqual System.getProperty("path.separator")
        }
    }

    "The fileSeparator" should {
        "be the platform's file separator" in {
            new Environment().fileSeparator shouldEqual System.getProperty("file.separator")
        }
    }

    "The lineSeparator" should {
        "be the platform's line separator" in {
            new Environment().lineSeparator shouldEqual System.getProperty("line.separator")
        }
    }

    "The currentWorkingDirectory" should {
        "be the user's current directory" in {
            new Environment().currentWorkingDirectory shouldEqual System.getProperty("user.dir")
        }
    }

    "The System Property getter" should {
        "return the system property" in {
            val expected = System.getProperty("java.class.path")
            Environment.getSystemProperty("java.class.path") shouldEqual expected
        }
    }

    "The System Property setter" should {
        "set the system property" in {
            val save = System.getProperty("java.class.path")
            val expected = save + System.getProperty("file.separator") + "/foo/bar"
            Environment.setSystemProperty("java.class.path", expected)
            Environment.getSystemProperty("java.class.path") shouldEqual expected
            Environment.setSystemProperty("java.class.path", save)
        }
    }

    "The System Environment Variables getter" should {
        "return the system environment, a map of defined variables" in {
            val expected = Some(System.getenv.get("CLASSPATH"))
            Environment.getSystemEnvironmentVariables.get("CLASSPATH") shouldEqual expected
        }
    }
}
