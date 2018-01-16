package Driver

object Driver {
  def main(args: Array[String]): Unit = {
    try {
      CommandLine parseArgs args
    } catch {
      case err: ExceptionHandling.Error => ExceptionHandling.Reporter.print( err )
    }
    
  }
}
