package sake.util

object ClassUtil {
    def toFullyQualifiedName(fileNameWithPath: String): String = {
        toFullyQualifiedName(fileNameWithPath, "")
    }

    def toFullyQualifiedName(fileNameWithPath: String, pathPrefixToRemove: String): String = {
        val fn = pathPrefixToRemove match {
            case "" => fileNameWithPath
            case prefix => {
                if (! fileNameWithPath.startsWith(prefix))
                    throw new BuildError("filename \""+fileNameWithPath+"\" does not have prefix \""+prefix+"\"")
                fileNameWithPath.replace(prefix, "")
            }
        }
        fn.replaceAll("/",".").replaceAll(".class$", "")
    }
}