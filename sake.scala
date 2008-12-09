import sake.Project

object project extends Project {
    
    classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    val srcDir = "src"
    val buildDir = "build"

    target('all -> List('clean, 'compile, 'spec))

    target('spec) {
        echo('message -> "spec")
        spec('files -> {buildDir+"/**/*Spec.class"})
    }

    target('compile) {
        echo('message -> "compile")
        scalac('files -> (srcDir+"/**/*.scala"), 'opts -> ("-d "+buildDir))
    }

    target('clean) {
        echo('message -> "clean")
        remove('files -> buildDir)
    }
}
