package Error

import scala.collection.mutable.ListBuffer

case class Report()

//TODO: Change reporter to use Trys instead
class Reporter {
  private var errors: ListBuffer[Error] = ListBuffer()
  private val errorFormatter: ErrorFormatter = new ErrorFormatter

//  override def receive: Receive = {
//    case e: Error =>
//      errors += e
//    case Report =>
//      for ( e <- errors ) print(errorFormatter.format(e)) // TODO: have this print in file order
//      sender() ! errors.nonEmpty
//  }
}
