package Driver

object CommandLine{
  private val validFlags = Set( "-dumpTokens" )
  var flags = Set[String]()
  private var _files = List[String]()
  def files = _files

  def parseArgs(line: Array[String]): Unit = {
    for ( str <- line ) {
      if ( str.contains(".java") ) _files = _files :+ str
      else if ( validFlags.contains(str) ) flags = flags + str
      else
        throw new ExceptionHandling.Error( str, "Unrecognized command line input",
          ExceptionHandling.Type.CommandLine )
    }
  }
}
