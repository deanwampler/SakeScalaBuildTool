package sake.util

case class BuildError(msg:String, th:Throwable) extends Exception(msg, th) {
    def this(msg:String) = this(msg, null)
}
