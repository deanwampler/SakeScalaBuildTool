import sake.Project._

// Define some convenient variables.
val srcDir   = "src/"
val buildDir = "build/"
val libDir   = "lib/"

// Version strings used for the generated jars:
// What version of Sake? Can specify on the command line with VERSION=...
val version = environment.getOrElse("VERSION", "1.1")
// What Scala version of Sake? Can specify on the command line with SCALA_VERSION=...
val scalaVersion = environment.getOrElse("SCALA_VERSION", "2.12.4")

// If true, don't actually run any commands.
environment.dryRun = false

// If true, show stack traces when a failure happens (doesn't affect "errors").
showStackTracesOnFailures = false

// Logging level: Info, Notice, Warn, Error, Failure
log.threshold = Level.Info

// Add to the classpath using list semantics.
environment.classpath ++= (files(libDir + "*.jar") -- files(libDir + "*src.jar"))
environment.classpath += buildDir

target('all -> List('clean, 'compile, 'spec, 'jars))

target('jars -> List('jar, 'srcjar))

target('jar) {
    val jarName = buildDir+"/sake-"+scalaVersion+"-"+version+".jar"
    sh("jar cf "+jarName+" -C "+buildDir+" sake")
    sh("cp "+jarName+" "+libDir+"/"+scalaVersion)
}

target('srcjar) {
    val jarName = buildDir+"/sake-"+scalaVersion+"-"+version+"-src.jar"
    sh("jar cf "+jarName+" -C "+buildDir+" sake")
    sh("cp "+jarName+" "+libDir+"/"+scalaVersion)
}

target('spec) {
    specs(
       'classpath -> environment.classpath,
       'path -> "./spec/**/*.scala",
       'pattern -> ".*Spec.*"
    )
}

target('compile -> List('clean, 'build_dir)) {
    scalac(
        'files     -> files(srcDir+"**/*.scala", specDir+"**/*.scala"),
        'classpath -> environment.classpath,
        'd         -> buildDir,
        'opts      -> ("-unchecked -deprecation") // -Xplugin:" + sxr +" -P:sxr:base-directory:.")
    )
}

target('clean) {
    deleteRecursively(buildDir)
}

target('build_dir) {
    mkdir(buildDir)
}

target('fail) {
    fail("boo!")
}

import sake.util._
target('ls) {
    shell('command -> "ls", 'opts -> ".", 'outputFile -> File("foo.txt"))
}
target('cat ) {
    shell('command -> "cat", 'inputFile -> File("foo.txt"), 'outputFile -> File("foo.txt2"))
}

