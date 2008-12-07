import sake.Predef._

object project {
    classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    val srcDir = "src"
    val buildDir = "build"

    target('all -> List('clean, 'compile, 'spec)) {}
    
    target('spec) {
       files("**/*Spec.class").foreach(scala(_))
    }

    target('compile) {
        scalac -cp ${CLASSPATH} *.scala
    }

    target('clean) {
       rm_dir(buildDir)
    }
}
