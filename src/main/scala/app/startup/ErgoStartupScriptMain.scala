package app.startup

import scopt.OptionParser

case class Config(
                   proxy: String = "",
                   tokenRepoTokenId: String = "",
                   receiverAddress: String = "",
                   amount: Long = 0L)

object ErgoExecutor extends App {
  val parser: OptionParser[Config] = new OptionParser[Config]("proxyStartup") {
    opt[String]('p', "proxy")
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
