package app.helpers

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, ErgoClient, NetworkType, RestApiErgoClient}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  lazy val explorerUrl: String = readKey("explorer.url")
  lazy val defaultTxFee: Long = readKey("default.fee").toLong
  lazy val ourAddress: String = readKey("our.address")
  lazy val proverSecret: String = readKey("prover.secret")
  lazy val consulsPk = Seq(readKey("consulsPk.one"), readKey("consulsPk.two"), readKey("consulsPk.three"),
    readKey("consulsPk.four"), readKey("consulsPk.five"))
  lazy val consulsPriv = Seq(readKey("consulsPriv.one"), readKey("consulsPriv.two"), readKey("consulsPriv.three"),
    readKey("consulsPriv.four"), readKey("consulsPriv.five"))
  lazy val oraclesPk = Seq(readKey("oraclesPk.one"), readKey("oraclesPk.two"), readKey("oraclesPk.three"),
    readKey("oraclesPk.four"), readKey("oraclesPk.five"))
  lazy val oraclesPriv = Seq(readKey("oraclesPriv.one"), readKey("oraclesPriv.two"), readKey("oraclesPriv.three"),
    readKey("oraclesPriv.four"), readKey("oraclesPriv.five"))
  lazy val ergoClient: ErgoClient = RestApiErgoClient.create(nodeUrl, networkType, nodeApiKey, explorerUrl)
}
