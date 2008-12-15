import sake.Project

import sake.util._

object project extends Project {
    
    val srcDir = "src/"
    val buildDir = "build/"

    environment.dryRun = true
    showStackTraces = false
    log.threshold = Level.Info
    environment.classpath ::= buildDir
    environment.classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 

    target('all -> List('clean, 'compile, 'spec))

    target('spec) {
        val specFiles = files(buildDir+"**/*Spec.class").map(ClassUtil.toFullyQualifiedName(_, buildDir))
        specFiles.foreach { s =>
            spec(
                'specs -> s,
                'classpath -> environment.classpath
            )
        }
    }

    target('compile -> 'clean) {
        scalac (
            'files  -> files(srcDir+"**/*.scala"),
            'classpath -> environment.classpath,
            'd      -> buildDir,
            'opts   -> "-unchecked -deprecation"
        )
    }

    target('clean) {
        remove_recursive('files -> buildDir)
    }
    
    target('files) {
        sh (
            'command -> "ls",
            'opts    -> files(buildDir+"/sake/**/*.class")
        )
        sh (
            'command -> "ls",
            'opts    -> files(buildDir+"/sake/**/*.class")
        )
    }
    
    target('echo) {
        echo("hello world!")
    }

    target('sh1) {
        sh (
            'command -> "touch",
            'opts    -> List("foobar.txt", "barfoo.txt")
        )
        sh (
            'command -> "ls",
            'opts    -> List("-l", "foobar.txt", "barfoo.txt")
        )
        sh ("ls -l bin")
        remove('files ->    "*bar*.txt")
    }
    target('dir) {
        sh (
            'command -> "ls",
            'opts    -> "-l ../.."
        )
        sh (
            'command -> "ls",
            'opts    -> ("-l" :: files("*"))
        )
    }
}
