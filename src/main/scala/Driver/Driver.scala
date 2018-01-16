package Driver

object Driver {
  def main(args: Array[String]): Unit = {
    try {
      CommandLine parseArgs args


    } catch {
      case err: Error.Error => Error.Reporter.print( err )
    }

  }
}
