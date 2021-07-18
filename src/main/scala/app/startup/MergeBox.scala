package app.startup

import app.helpers.Configs
import app.startup.Gateway.our
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.sigma.GroupElement

import java.io.PrintWriter
import java.math.BigInteger
import scala.collection.JavaConverters._
import scala.util.control.Breaks._

object MergeBox {
  val secureRandom = new java.security.SecureRandom
  val our = Address.create(Configs.ourAddress)

  def run(ctx: BlockchainContext): Unit = {
    val secret = BigInt(Configs.proverSecret, 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()
    object AllDone extends Exception {}
    breakable {
      while (true) {
        try {
          var boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getValue == 1000000L)
          if (boxes.size <= 1) {
            throw AllDone
          }
          var ergBox = boxes
          if (boxes.size >= 10){
            ergBox = boxes.slice(0,10)
          }
          else {
            ergBox = boxes
          }
          val txB = ctx.newTxBuilder()

          val newBox = txB.outBoxBuilder
            .value((ergBox.size - 1) * Configs.defaultTxFee)
            .contract(new ErgoTreeContract(our.getErgoAddress.script))
            .build()

          val tx = txB.boxesToSpend(ergBox.asJava)
            .outputs(newBox)
            .fee(Configs.defaultTxFee)
            .sendChangeTo(our.getErgoAddress)
            .build()

          val signed: SignedTransaction = prover.sign(tx)
          println(signed.toJson(false))
          val txId = ctx.sendTransaction(signed)
          println(txId)

        }
        catch {
          case e: Exception => println(e); break
        }
      }
    }
  }
}
