import sake._
import scala.sys.process._

val sakeBuild = new Project {
  val settings = defaultSettings.copy(
    version = "2.0",
    scalaVersion = "2.12.4",
    userSettings = Map("foo" -> "bar"),
    dryRun = false,
    logThreshold = Log.Level.Info,
    showStackTracesOnFailures = true)

  target("all" -> Seq("clean", "compile", "test", "jars"))

  val srcfiles:  Seq[File] = filesRecursive(root = settings.srcDir,  glob = "*.scala")
  val testfiles: Seq[File] = filesRecursive(root = settings.testDir, glob = "*.scala")

  val binjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}.jar"
  val srcjar: File = settings.targetDir / s"sake-${settings.scalaVersion}-${settings.version}-src.jar"))

  target("jars" -> Seq(binjar, srcjar))

  file(binjar) { context =>
    jar(
      jarName(context.target),
      sourceDirs(settings.targetDir / "classes"))
  }

  file(srcjar) { context =>
    jar(
      jarName(context.target),
      sourceDirs(settings.sourceDir, settings.testDir))
  }

  target("test" -> "compile") { context =>
    scalatest(
       classpath(settings.classpath),
       files(testfiles),
       pattern(".*Spec.*"))
  }

  target("compile" -> Seq("clean", dir(settings.targetdir))) { context =>
    scalac(
      files(srcfiles ++ testfiles),
      classpath(properties.classpath),
      targetDir(properties.targetDir),
      unchecked,
      deprecation)
  }

  target("clean" -> Seq(clean(settings.targetDir))

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
