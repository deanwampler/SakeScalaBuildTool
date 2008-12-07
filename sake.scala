import sake.Predef._

object project {
    classpath ::= "/Library/tools/scala/scala-specs/specs-1.4.1.jar" 
    val srcDir = "src"
    val buildDir = "build"

    target('all -> List('clean, 'compile, 'spec))
    
    target('spec) build {
       files("**/*Spec.class").foreach(scala(_))
    }

    target('compile) build {
        scalac -cp ${CLASSPATH} *.scala
    }

    target('clean) build {
       rm_dir(buildDir)
    }
}
