import sake._
import sake.files.File
import sake.util.Log
import scala.sys.process._

val sakeBuild = new Project("sakeBuild") {
  import implicits._

  settings = settings.copy(
    version = "2.0",
    scalaVersion = "2.12.4",
    dryRun = false,
    logThreshold = Log.Level.Info,
    showStackTracesOnFailures = true,
    custom = Map("foo" -> "bar"))

  target("all").after(Seq("clean", "compile", "test", "jars"))

  val srcfiles:  Seq[File] = findRecursive(settings.srcDirs,  "*.scala")
  val testfiles: Seq[File] = findRecursive(settings.testDirs, "*.scala")

  val binjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}.jar"
  val srcjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}-src.jar"

  target("jars").after(Seq(binjar, srcjar))

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

  target("test").deps("compile") { context =>
    // scalatest(
    //    classpath(settings.classpath),
    //    files(testfiles),
    //    pattern(".*Spec.*"))
    success
  }

  target("compile").after(Seq("clean", mkdir(settings.targetDir))) { context =>
    // scalac(
    //   files(srcfiles ++ testfiles),
    //   classpath(properties.classpath),
    //   targetDir(properties.targetDir),
    //   unchecked,
    //   deprecation)
    success
  }

  target("clean").after(clean(settings.targetDir))

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
