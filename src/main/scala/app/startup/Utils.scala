package app.startup

import app.helpers.Configs
import org.ergoplatform.appkit.BlockchainContext

object Utils {
  def execute(configs: Config): Unit = {
    Configs.ergoClient.execute((ctx: BlockchainContext) => {
      configs.proxy.toLowerCase match {
        case "gateway" => Gateway.run(ctx)
        case "susy" => Susy.run(ctx, configs.tokenRepoTokenId)
      }
    })
  }
}
