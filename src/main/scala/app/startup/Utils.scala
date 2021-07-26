package app.startup

import app.helpers.Configs
import app.helpers.Configs.{explorerUrl, networkType, nodeApiKey, nodeUrl}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoClient, RestApiErgoClient}

object Utils {
  val ergoClient: ErgoClient = RestApiErgoClient.create(nodeUrl, networkType, nodeApiKey)
  def execute(configs: Config): Unit = {

    ergoClient.execute((ctx: BlockchainContext) => {
      configs.proxy.toLowerCase match {
        case "gateway" => Gateway.run(ctx)
        case "susy" => Susy.run(ctx)
      }
    })
  }
}
