import sake.Predef._

object project {
    classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    val srcDir = "src"
    val buildDir = "build"

    target('all -> List('clean, 'compile, 'spec))
    
    target('spec) action {
       files("**/*Spec.class").foreach(scala(_))
    }

    target('compile) action {
        scalac -cp ${CLASSPATH} *.scala
    }

    target('clean) action {
       rm_dir(buildDir)
    }
}
