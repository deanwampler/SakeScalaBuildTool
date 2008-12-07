package sake {

    case class BuildError(msg:String) extends Exception(msg)
}