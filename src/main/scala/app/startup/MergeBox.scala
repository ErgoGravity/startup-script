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
    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getValue == Configs.defaultTxFee)
    val theBox = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getValue > 10 * Configs.defaultTxFee).head

    try {
      //
      println(boxes.size)
      if (boxes.size <= 1) {
        throw AllDone
      }

      for (i <- 1 to (boxes.size / 2)) {
        var ergBox = boxes.slice(i, i + 2)

        val txB = ctx.newTxBuilder()

        val newBox = txB.outBoxBuilder
          .value(ergBox.size * Configs.defaultTxFee)
          .contract(new ErgoTreeContract(our.getErgoAddress.script))
          .build()

        val feeBox = txB.outBoxBuilder
          .value(theBox.getValue - Configs.defaultTxFee)
          .contract(new ErgoTreeContract(our.getErgoAddress.script))
          .build()

        val tx = txB.boxesToSpend((Seq(theBox) ++ ergBox).asJava)
          .outputs(newBox, feeBox)
          .fee(Configs.defaultTxFee)
          .sendChangeTo(our.getErgoAddress)
          .build()

        val signed: SignedTransaction = prover.sign(tx)
        println(signed.toJson(false))
        val txId = ctx.sendTransaction(signed)
        println(txId)
      }

    }
    catch {
      case e: Exception => println(e); break
    }
  }

}
