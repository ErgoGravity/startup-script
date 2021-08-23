package app.helpers

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, ErgoClient, NetworkType, RestApiErgoClient}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  private lazy val explorerUrlConf = readKey("explorer.url", "")
  lazy val explorerUrl: String = if (explorerUrlConf.isEmpty) RestApiErgoClient.getDefaultExplorerUrl(Configs.networkType) else explorerUrlConf
  lazy val defaultTxFee: Long = readKey("default.fee").toLong

  lazy val ourAddress: String = readKey("our.address")
  lazy val proverSecret: String = readKey("prover.secret")

  lazy val ergoTokenRepoTokenId: String = readKey("ergo.tokenRepoToken.id")
  lazy val sigmaTokenRepoTokenId: String = readKey("sigma.tokenRepoToken.id")
  lazy val sWTokenId: String = readKey("SWToken.id ")
  lazy val gwSWTokenId: String = readKey("gwSWToken.id ")

  lazy val consulsAddress: java.util.List[String] = readobject("consulsAddress")
  lazy val consulsPrivateKey: java.util.List[String] = readobject("consulsPrivateKey")
  lazy val oraclesAddress: java.util.List[String] = readobject("oraclesAddress")
  lazy val oraclesPrivateKey: java.util.List[String] = readobject("oraclesPrivateKey")

}
