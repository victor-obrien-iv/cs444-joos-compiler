package Driver

import Error.ErrorFormatter

class CommandLine(line: Array[String], errorFormatter: ErrorFormatter){
  private val validFlags = Set( "-dumpTokens" )
  val files: Array[String] = line.filter(_.contains(".java"))
  val flags: Set[String] = line.filter(validFlags.contains).toSet

  try {
    for( str <- line )
      if ( !(flags ++ files.toSet).contains(str) )
        throw Error.Error( str,
          "Unrecognized command line input", Error.Type.CommandLine )

    if ( files.isEmpty )
      throw Error.Error( "", "No .java file specified", Error.Type.CommandLine )
  }
  catch {
    case e: Error.Error => println(errorFormatter.format(e))
  }

}
