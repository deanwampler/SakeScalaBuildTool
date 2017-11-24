import sake._
import scala.sys.process._

val sakeBuild = new Project {
  settings = settings.copy(
    version = "2.0",
    scalaVersion = "2.12.4",
    userSettings = Map("foo" -> "bar"),
    dryRun = false,
    logThreshold = Log.Level.Info,
    showStackTracesOnFailures = true)

  target('all -> Seq('clean, 'compile, 'test, 'jars))

  target('jars -> Seq('jar, 'srcjar))

  target('jar) { context =>
    val jarName = s"${targetDir}/sake-${scalaVersion}-${version}.jar"
    sh(s"jar cf ${jarName} -C ${targetDir} sake")
    sh(s"cp ${jarName} ${libDir}/${scalaVersion}")
  }

  target('srcjar) { context =>
    val jarName = s"${targetDir}/sake-${scalaVersion}-${version}-src.jar"
    sh(s"jar cf ${jarName} -C ${srcDir} sake")
    sh(s"cp ${jarName} ${libDir}/${scalaVersion}")
  }

  target('test) { context =>
    specs(
       'classpath -> environment.classpath,
       'path -> "./src/test/scala/**/*.scala",
       'pattern -> ".*Spec.*"
    )
  }

  target('compile -> Seq('clean, 'target_dir)) { context =>
    val flags = "-unchecked -deprecation"
    val files = files(s"${srcDir}/**/*.scala", s"${testDir}/**/*.scala")
    sh("scalac -d ${targetDir} -classpath ${Properties.classpath} ${flags} ${files}")

    // scalac(
    //   files(properties.srcDir / "**/*.scala", properties.testDir / "**/*.scala"),
    //   classpath(properties.classpath),
    //   targetDir(properties.targetDir),
    //   unchecked,
    //   deprecation)
  }

  target('clean) { context =>
      deleteRecursively(targetDir)
  }

  target('target_dir) { context =>
      mkdir(targetDir)
  }

  target('fail) { context =>
      fail("boo!")
  }

  import sake.util._
  target('ls) { context =>
      sh(s"ls . > foo.txt")
  }
  target('cat ) { context =>
      sh("cat foo.txt > foo.txt2")
  }

}
