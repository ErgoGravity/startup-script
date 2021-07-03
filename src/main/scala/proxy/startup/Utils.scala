package proxy.startup

import org.ergoplatform.appkit.BlockchainContext
import proxy.startup.Gateway.ergoClient
import Gateway._

object Utils {
  def execute(configs: Config): Unit ={
    ergoClient.execute((ctx: BlockchainContext) => {
      configs.proxy.toLowerCase match {
        case "gateway" => Gateway.run(ctx)
//        case "susy" => Susy.run(ctx)
      }
    })
  }

}
