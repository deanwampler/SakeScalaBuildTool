import sake.Project._

// Define some convenient variables.
val srcDir   = "src/"
var specDir  = "spec/"
val buildDir = "build/"
val libDir   = "lib/"

// If true, don't actually run any commands.
environment.dryRun = false

// If true, show stack traces when a failure happens (doesn't affect "errors").
showStackTracesOnFailures = false

// Logging level: Info, Notice, Warn, Error, Failure
log.threshold = Level.Info

// Add to the classpath using list semantics.
environment.classpath ::= buildDir
files(libDir+"*.jar") foreach { lib => environment.classpath ::= lib }

target('all -> List('clean, 'compile, 'spec, 'jar))

target('jar) {
    sh("jar cf "+buildDir+"/sake.jar -C "+buildDir+" sake")
    sh("cp "+buildDir+"/sake.jar "+libDir)
}

target('spec) {
   specs('path -> "spec", 'pattern -> ".*")
}

target('compile -> List('clean, 'build_dir)) {
    scalac(
        'files     -> files(srcDir+"**/*.scala", specDir+"**/*.scala"),
        'classpath -> environment.classpath,
        'd         -> buildDir,
        'opts      -> "-unchecked -deprecation"
    )
}

target('fail) {
    fail("boo!")
}

target('clean) {
    deleteRecursively(buildDir)
}

target('build_dir) {
    mkdir(buildDir)
}
