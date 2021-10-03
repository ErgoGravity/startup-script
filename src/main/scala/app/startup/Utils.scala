package app.startup
import io.circe.Json
import app.helpers.{Configs, Explorer}
import app.helpers.Configs.{explorerUrl, networkType, nodeApiKey, nodeUrl}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoClient, InputBox, RestApiErgoClient}

object Utils {
  val ergoClient: ErgoClient = RestApiErgoClient.create(nodeUrl, networkType, nodeApiKey, explorerUrl)
  final case class explorerException(private val message: String = "Explorer error") extends Throwable(message)
  final case class connectionException(private val message: String = "Network Error") extends Throwable(message)
  final case class parseException(private val message: String = "Parsing failed") extends Throwable(message)

  def findMempoolBox(address: String): List[String] = {
    try {
      val mempool = Explorer.getUnconfirmedTxByAddress(address)
      val mempoolList = mempool.hcursor.downField("items").as[List[Json]].getOrElse(null)

      var idList = List[String]()
      for (items <- mempoolList) {
        val inputs = items.hcursor.downField("inputs").as[List[Json]].getOrElse(null)
        for (ids <- inputs) {
          idList = idList :+ ids.hcursor.downField("id").as[String].getOrElse(null)
        }
      }
      idList
    } catch {
      case e: explorerException => {
        throw connectionException()
      }
      case _: parseException => throw connectionException()
      case e: Throwable => {
        throw new Throwable("Something is wrong")
      }
    }
  }

  println(Configs.nodeUrl)
  def execute(configs: Config): Unit = {

    ergoClient.execute((ctx: BlockchainContext) => {
      configs.proxy.toLowerCase match {
        case "gateway" => Gateway.run(ctx)
        case "susy" => Susy.run(ctx)
      }
    })
  }
}
