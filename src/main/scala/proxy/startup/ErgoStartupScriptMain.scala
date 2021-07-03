package proxy.startup

import scopt.OptionParser

case class Config(
    proxy: String = "",
    ibportAddress: String = "",
    receiverAddress: String = "",
    amount: Long = 0L)

object ErgoExecutor extends App {
  val parser: OptionParser[Config] = new OptionParser[Config]("proxyStartup") {
    opt[String]("proxy")
      .action((x, c) => c.copy(proxy = x))
      .text("gateway or susy")
      .required()

    help("help").text("prints this usage text")
  }

  parser.parse(args, Config()) match {
    case Some(config) =>
        Utils.execute(config)
    case None =>
    // arguments are bad, error message will have been displayed
  }
}
