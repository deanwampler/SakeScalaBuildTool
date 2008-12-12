import sake.Project

object project extends Project {
    
    val srcDir = "src"
    val buildDir = "build"

    environment.dryRun = false
    showStackTraces = false
    log.threshold = Level.Info
    environment.classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    environment.classpath ::= buildDir

    target('all -> List('clean, 'compile, 'spec))

    target('spec) {
        spec(
            'specs -> "sake.util.PathSpec",
            'classpath -> environment.classpath
        )
    }

    target('compile -> 'clean) {
        scalac (
            'files -> (srcDir+"/**/*.scala"),
            'classpath -> environment.classpath,
            'd     -> buildDir
        )
    }

    target('clean) {
        remove_recursive('files -> buildDir)
    }
    
    target('echo) {
        echo("hello world!")
    }

    target('sh1) {
        sh (
            'command -> "touch",
            'opts    -> "foobar.txt"
        )
        sh (
            'command -> "ls",
            'opts    -> "-l"
        )
        sh ("ls -l bin")
        remove('files -> "foobar.txt")
    }
}
