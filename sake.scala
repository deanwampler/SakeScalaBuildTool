import sake.Project

import sake.util._

object project extends Project {
    
    val srcDir = "src/"
    val buildDir = "build/"

    environment.dryRun = false
    showStackTracesOnFailures = false
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

    target('compile -> List('clean, 'build_dir)) {
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
    
    target('build_dir) {
        mkdir(buildDir)
    }
}
