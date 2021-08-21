package app.startup

import scala.util.control.Breaks._

object test {
  def runMe(): Unit = {
    var methodName: String = "";
    var stacktrace = Thread.currentThread().getStackTrace;
    breakable {
      for (i <- 0 to stacktrace.length) {
        if (stacktrace(i).getMethodName.equals("runMe")) {
          methodName = stacktrace(i + 1).getMethodName;
          break
        }
      }
    }
    println(methodName)
  }

  def run(): Unit = {
    runMe()
  }

  def runner(): Unit = {
    runMe()
  }

  def main(): Unit = {
    run()
  }
}
