package Driver

object CommandLine{
  private val validFlags = Set( "-dumpTokens" )
  var flags: Set[String] = Set[String]()
  private var _files = List[String]()
  def files: List[String] = _files

  def parseArgs(line: Array[String]): Unit = {
    for ( str <- line ) {
      if ( str.contains(".java") ) _files = _files :+ str
      else if ( validFlags.contains(str) ) flags = flags + str
      else
        throw Error.Error( str, "Unrecognized command line input",
          Error.Type.CommandLine )
    }
  }
}
