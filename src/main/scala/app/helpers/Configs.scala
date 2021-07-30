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
  lazy val SWTokenId : String = readKey("SWToken.id ")
  lazy val gwSWTokenId: String = readKey("gwSWToken.id ")
  lazy val consulsPk = Seq(readKey("consulsPk.one"), readKey("consulsPk.two"), readKey("consulsPk.three"),
    readKey("consulsPk.four"), readKey("consulsPk.five"))
  lazy val consulsPriv = Seq(readKey("consulsPriv.one"), readKey("consulsPriv.two"), readKey("consulsPriv.three"),
    readKey("consulsPriv.four"), readKey("consulsPriv.five"))
  lazy val oraclesPk = Seq(readKey("oraclesPk.one"), readKey("oraclesPk.two"), readKey("oraclesPk.three"),
    readKey("oraclesPk.four"), readKey("oraclesPk.five"))
  lazy val oraclesPriv = Seq(readKey("oraclesPriv.one"), readKey("oraclesPriv.two"), readKey("oraclesPriv.three"),
    readKey("oraclesPriv.four"), readKey("oraclesPriv.five"))
  }
