package Driver

import akka.actor.ActorRef

class CommandLine(line: Array[String], reporter: ActorRef){
  private val validFlags = Set( "-dumpTokens" )
  val files: Array[String] = line.filter(_.contains(".java"))
  val flags: Set[String] = line.filter(validFlags.contains).toSet

  for( str <- line )
    if ( !(flags ++ files.toSet).contains(str) )
      reporter ! Error.Error( str,
        "Unrecognized command line input", Error.Type.CommandLine )

  if ( files.isEmpty )
    reporter ! Error.Error( "", "No .java file specified", Error.Type.CommandLine )
}
