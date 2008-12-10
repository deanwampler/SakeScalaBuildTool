import sake.Project

object project extends Project {
    
    environment.dryRun = false //true
    showStackTraces = false
    log.threshold = Level.Info
    environment.classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    val srcDir = "src"
    val buildDir = "build"

    target('all -> List('clean, 'compile, 'spec))

    target('spec) {
        spec('files -> {buildDir+"/**/*Spec.class"})
    }

    target('compile -> 'clean) {
        scalac('files -> (srcDir+"/**/*.scala"), 'opts -> ("-d "+buildDir))
    }

    target('clean) {
        remove_recursive('files -> buildDir)
    }

    target('sh1) {
        sh('command -> "touch foobar.txt")
        sh('command -> """[ "X" = "Y" ]""")
        sh('command -> """find . -name '*.scala' -exec grep -H \'must be_\' {} \;""")
    }
}
