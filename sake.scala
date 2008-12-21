import sake.Project._
import sake.util._

val srcDir   = "src/"
var specDir  = "spec/"
val buildDir = "build/"

environment.dryRun = false
showStackTracesOnFailures = false
log.threshold = Level.Info
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
