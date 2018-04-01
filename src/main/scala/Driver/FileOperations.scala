package Driver

object FileOperations {
  def getPath(filename: String): String = {
    val lastSlash = filename.lastIndexWhere((c: Char) => c == '/' || c == '\\')
    if( lastSlash > 0 ) filename.substring(0, lastSlash + 1)
    else "./"
  }

  def getFileBaseName(filename: String): String = {
    val lastSlash = filename.lastIndexWhere((c: Char) => c == '/' || c == '\\')
    filename.substring(
      if( lastSlash > 0 ) lastSlash + 1 else 0,   // remove the path ahead of the file
      filename.length - 5                         // remove the .java from the end
    )
  }
}
