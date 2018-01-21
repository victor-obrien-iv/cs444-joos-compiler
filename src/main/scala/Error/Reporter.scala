package Error

object Reporter {
  def print(err: Error): Unit = {
    val str = err.kind match {
      case Type.CommandLine => "An error occurred with command line argument: " + err.cause
    }
    println( str )
    if ( err.loc.isDefined ){
      // TODO print line in file pointing to violating char
    }
    println( err.msg )
  }
}
