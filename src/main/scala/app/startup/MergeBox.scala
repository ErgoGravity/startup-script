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

    try {
      //
      val we = Address.create("4B8vRwhxhAJb8TuN4RZpBeLmAkQUtf7a4QaYqqSCZAQHjyhzgVLGiFTPp9CaZRbssRnVHAzqUKtBBCWjfMSxpi1Z59H7sj1E8o6XFqYCRYFp1n2aC6tYptFLrrX3c1jXBhcsS7NNU5NLHSuCNC6bDf9KYhxfgQTtYkozrGk2qzB3aB8mmaC8ZHaAfzvQmSzcxGwtTyquk4yfeR9tsvrvhGPeyv6Ys74Q3VqQoVkuSMq21gRY5wgBz4gq6tTBY1Xv4BkHoFQm2wx8TWQc8Rof81DCePanX598Z9ZJzaDLnT5TDjQpECKPXvuuxLivbbFjtc4FZ65xTSHGk2uHsohRAMrMiug5aVa7UXwoRRLomvCrg8MbyowkuiQXcZWu8ue3QBTLuL5yy6yqxs2SYDPiFHqJhU3ag94Tb5Q2bPaJcAgomJehtn9BB5L9GoLe6Mx9Jti6F1WXgrGcuHeBQnXpiS8pRMN845AxseExzyvksmj4gyXEJqgookbneHtPuDW3QefeWH4LH13AvmXhm8QFTYdAFRuhXMBwJJbXL1yBcLE4nexa7d2AY7Hndr7gfecxRY4UnypdpLnpTTec5vEppaDrEx6KQ4gdeiDyJLmJCGHeB")
      println(boxes.size)
      if (boxes.size <= 1) {
        throw AllDone
      }
      var ergBox = boxes

      for (i <- 1 to (boxes.size / 10)) {
        ergBox = boxes.slice(i, i + 10)

        val txB = ctx.newTxBuilder()

        val newBox = txB.outBoxBuilder
          .value((ergBox.size - 2) * Configs.defaultTxFee)
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

    }
    catch {
      case e: Exception => println(e); break
    }
  }

}
