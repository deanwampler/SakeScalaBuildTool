import sake.Project._

// Define some convenient variables.
val srcDir   = "src/"
var specDir  = "spec/"
val buildDir = "build/"

// If true, don't actually run any commands.
environment.dryRun = false

// If true, show stack traces when a failure happens (doesn't affect "errors").
showStackTracesOnFailures = false

// Logging level: Info, Notice, Warn, Error, Failure
log.threshold = Level.Info

// Add to the classpath using list semantics.
environment.classpath ::= buildDir
environment.classpath ::= "lib/specs-1.4.1.jar:lib/junit-4.4.jar:lib/sake.jar" 

target('all -> List('clean, 'compile, 'spec))

target('spec) {
   specs('path -> "spec", 'pattern -> ".*")
}

target('compile -> List('clean, 'build_dir)) {
    scalac (
        'files     -> files(srcDir+"**/*.scala", specDir+"**/*.scala"),
        'classpath -> environment.classpath,
        'd         -> buildDir,
        'opts      -> "-unchecked -deprecation"
    )
}

target('clean) {
    deleteRecursively(buildDir)
}

target('build_dir) {
    mkdir(buildDir)
}
