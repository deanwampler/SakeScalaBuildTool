import sake._
import sake.files.File
import sake.util.Log
import scala.sys.process._

val sakeBuild = new Project("sakeBuild") {

  settings = settings.copy(
    version = "2.0",
    scalaVersion = "2.12.4",
    dryRun = false,
    logThreshold = Log.Level.Info,
    showStackTracesOnFailures = true,
    other = Map("foo" -> "bar"))

  target("all" -> Seq("clean", "compile", "test", "jars"))

  val srcfiles:  Seq[File] = findInDirRecursive(settings.srcDirs,  "*.scala")
  val testfiles: Seq[File] = findInDirRecursive(settings.testDirs, "*.scala")

  val binjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}.jar"
  val srcjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}-src.jar"

  target("jars" -> Seq(binjar, srcjar))

  file(binjar) { context =>
    // jar(
    //   jarName(context.target),
    //   sourceDirs(settings.targetDir / "classes"))
    success
  }

  file(srcjar) { context =>
    // jar(
    //   jarName(context.target),
    //   sourceDirs(settings.sourceDir, settings.testDir))
    success
  }

  target("test" -> "compile") { context =>
    // scalatest(
    //    classpath(settings.classpath),
    //    files(testfiles),
    //    pattern(".*Spec.*"))
    success
  }

  target("compile" -> Seq("clean", mkdir(settings.targetdir))) { context =>
    // scalac(
    //   files(srcfiles ++ testfiles),
    //   classpath(properties.classpath),
    //   targetDir(properties.targetDir),
    //   unchecked,
    //   deprecation)
    success
  }

  target("clean" -> Seq(clean(settings.targetDir)))

  target("fail") { context =>
    fail("boo!")
  }

  target("ls") { context =>
    "ls . > foo.txt".!
  }

  target("cat", "echo") { context =>
    s"${context.target} sake.scala > sake.scala"!
  }
}
