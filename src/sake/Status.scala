package sake

class Status(val passed:Boolean, val message:String)

object Status {
    val OK = new Status(true, "")
}