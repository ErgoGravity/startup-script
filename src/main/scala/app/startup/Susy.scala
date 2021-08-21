package app.startup

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.sigma.GroupElement
import app.helpers.Configs
import scala.util.control.Breaks._

import java.io.PrintWriter
import java.math.BigInteger
import scala.collection.JavaConverters._

object Susy {
  val addrEnc = new ErgoAddressEncoder(NetworkType.MAINNET.networkPrefix)
  val secureRandom = new java.security.SecureRandom
  val our = Address.create(Configs.ourAddress)

  def randBigInt: BigInt = new BigInteger(256, secureRandom)

  def toByteArray(s: String): Array[Byte] = Base16.decode(s).get

  def sign(msg: Array[Byte], sk: BigInt): (GroupElement, special.sigma.BigInt) = {
    val r = randBigInt
    val g: GroupElement = CryptoConstants.dlogGroup.generator
    val a: GroupElement = g.exp(r.bigInteger)
    val z = (r + sk * BigInt(scorex.crypto.hash.Blake2b256(msg))) % CryptoConstants.groupOrder
    if (z.bigInteger.bitLength() < 256) (a, JavaHelpers.SigmaDsl.BigInt(z.bigInteger)) else sign(msg, sk)
  }

  def getProveDlogAddress(z: BigInt, ctx: BlockchainContext): String = {
    val g: GroupElement = CryptoConstants.dlogGroup.generator
    val gZ: GroupElement = g.exp(z.bigInteger)
    val contract = ctx.compileContract(
      ConstantsBuilder.create()
        .item(
          "gZ", gZ
        ).build(), "{proveDlog(gZ)}"
    )
    addrEnc.fromProposition(contract.getErgoTree).get.toString
  }

  def issueNFTToken(prover: ErgoProver, box: InputBox, tokenName: String, tokenDescription: String): (String, String) = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {
      val txB = ctx.newTxBuilder()
      val id = box.getId.toString
      val issuingNum = 1L
      val newBox = txB.outBoxBuilder
        .value(Configs.defaultTxFee)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")), ErgoValue.of(Array(1.toByte)))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(our.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(Configs.defaultTxFee)
        .sendChangeTo(our.getErgoAddress)
        .build()

      var methodName: String = "";
      val stacktrace = Thread.currentThread().getStackTrace;
      breakable {
        for (i <- 0 to stacktrace.length) {
          if (stacktrace(i).getMethodName.equals("issueNFTToken")) {
            methodName = stacktrace(i + 1).getMethodName;
            break
          }
        }
      }
      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      new PrintWriter(s"result_susy/${tokenName}_signed_${methodName}.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_susy/${tokenName}_txId_${methodName}.txt") {
        write(txId)
        close()
      }
      println(txId)
      (id, signed.getOutputsToSpend.get(0).getId.toString)
    })
    //    tokenId
  }

  def issueToken(prover: ErgoProver, box: InputBox, tokenName: String, tokenDescription: String, issuingNum: Long = 1000000L): (String, String) = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {

      val txB = ctx.newTxBuilder()
      val id = box.getId.toString
      val newBox = txB.outBoxBuilder
        .value(Configs.defaultTxFee)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(our.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(Configs.defaultTxFee)
        .sendChangeTo(our.getErgoAddress)
        .build()

      var methodName: String = "";
      val stacktrace = Thread.currentThread().getStackTrace;
      breakable {
        for (i <- 0 to stacktrace.length) {
          if (stacktrace(i).getMethodName.equals("issueToken")) {
            methodName = stacktrace(i + 1).getMethodName;
            break
          }
        }
      }
      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      new PrintWriter(s"result_susy/${tokenName}_signed_${methodName}.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_susy/${tokenName}_txId_${methodName}.txt") {
        write(txId)
        close()
      }
      println(txId)
      (id, signed.getOutputsToSpend.get(0).getId.toString)
    })
    //    tokenId
  }

  def randomAddr(): Unit = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {
      val rnd = randBigInt
      println(s"secret: ${rnd.toString(16)}")
      val addr = getProveDlogAddress(rnd, ctx)
      println(s"pk/address: ${addr}")
    })
  }

  def createLinkListBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: List[InputBox], linkListRepoContract: ErgoContract, linkListTokenBoxId: String, linkListTokenRepoBoxId: String): Unit = {
    val tokenNFTBox = ctx.getBoxesById(linkListTokenBoxId).head
    val tokenBox = ctx.getBoxesById(linkListTokenRepoBoxId).head
    val txB = ctx.newTxBuilder()


    var boxes = boxFee.filter(box => box.getValue > 100 * Configs.defaultTxFee)

    var ergBox: List[InputBox] = List()
    var total = 0L
    object AllDone extends Exception {}

    val maxValue = Configs.defaultTxFee * (tokenBox.getTokens.get(0).getValue + 4)
    while (total < maxValue) {
      total = 0L
      for (box <- boxes) {
        total += box.getValue
        ergBox = box :: ergBox
        if (total == maxValue) {
          throw AllDone
        }
      }
      if (total < maxValue) {
        println("Not enough erg, waiting for more ergs ...")
        Thread.sleep(2 * 60 * 1000)
        ergBox = List()
        boxes = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList
          .filter(box => box.getTokens.size() == 0 && box.getValue > 100 * Configs.defaultTxFee)
      }
    }

    def createLinkListRepo(txB: UnsignedTransactionBuilder, tokenNFTBox: InputBox, tokenBox: InputBox, linkListRepoContract: ErgoContract): OutBox = {
      txB.outBoxBuilder
        .value(Configs.defaultTxFee * (tokenBox.getTokens.get(0).getValue + 1))
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue),
          new ErgoToken(tokenNFTBox.getTokens.get(0).getId, 1))
        .contract(linkListRepoContract)
        .build()
    }

    def createChangeBoxes(txB: UnsignedTransactionBuilder, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(total - Configs.defaultTxFee * (tokenBox.getTokens.get(0).getValue + 2))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend((Seq(tokenNFTBox, tokenBox) ++ ergBox).asJava)
      .outputs(createLinkListRepo(txB, tokenNFTBox, tokenBox, linkListRepoContract),
        createChangeBoxes(txB, our))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(our.getErgoAddress)
      .build()

    var methodName: String = "";
    val stacktrace = Thread.currentThread().getStackTrace;
    breakable {
      for (i <- 0 to stacktrace.length) {
        if (stacktrace(i).getMethodName.equals("createLinkListBox")) {
          methodName = stacktrace(i + 1).getMethodName;
          break
        }
      }
    }
    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter(s"result_susy/LinkListRepoBox_signed_${methodName}.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter(s"result_susy/LinkListRepoBox_tx_${methodName}.txt") {
      write(txId);
      close()
    }
    println(txId)
  }


  def createMaintainerBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, maintainerRepoContract: ErgoContract, maintainerTokenBoxId: String, TokenId: String): Unit = {
    val tokenNFTBox = ctx.getBoxesById(maintainerTokenBoxId).head

    val tokenBox = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(TokenId)
    }).head

    val txB = ctx.newTxBuilder()

    def createMaintainerRepo(txB: UnsignedTransactionBuilder, tokenNFTBox: InputBox, tokenBox: InputBox, maintainerRepoContract: ErgoContract): OutBox = {
      val fee: Int = 0
      txB.outBoxBuilder
        .value(tokenNFTBox.getValue + tokenBox.getValue)
        .tokens(new ErgoToken(tokenNFTBox.getTokens.get(0).getId, 1),
          new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(ErgoValue.of(fee))
        .contract(maintainerRepoContract)
        .build()
    }

    def createChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - 1))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(tokenNFTBox, tokenBox, boxFee).asJava)
      .outputs(createMaintainerRepo(txB, tokenNFTBox, tokenBox, maintainerRepoContract),
        createChangeBoxes(txB, boxFee, Configs.defaultTxFee, our))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(our.getErgoAddress)
      .build()

    var methodName: String = "";
    val stacktrace = Thread.currentThread().getStackTrace;
    breakable {
      for (i <- 0 to stacktrace.length) {
        if (stacktrace(i).getMethodName.equals("createMaintainerBox")) {
          methodName = stacktrace(i + 1).getMethodName;
          break
        }
      }
    }
    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter(s"result_susy/MaintainerBox_signed_${methodName}.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter(s"result_susy/MaintainerBox_txId_${methodName}.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def runLUPort(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    val secret = BigInt(Configs.proverSecret, 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()
    val linkListRepoScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(0).tokens(0)._2 - 1,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(0).value - minValue,//TODO : check minvalue
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT  ){ // approve
         |      val linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(2).tokens(0)._2 + 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(2).value + minValue,
         |
         |        INPUTS(2).propositionBytes == SELF.propositionBytes,
         |        INPUTS(2).id == SELF.id,
         |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
         |      ))
         |    }
         |    else false
         |  }
         |
         |  sigmaProp (check)
         |}""".stripMargin
    val maintainerRepoScript: String =
      s"""{
         |val storeInMaintainer = {(v: ((Box, Box), BigInt )) => {
         |    if (v._1._1.tokens.size > 1){
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value,
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2
         |      ))
         |    }
         |    else{
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value + v._2
         |      ))
         |    }
         |  }}
         |
         |val unlock: Boolean = {(v: ((Box, Box), (Box, BigInt))) => {
         |  if (v._1._1.tokens.size > 1){
         |    allOf(Coll(
         |      v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |      v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2,
         |      v._1._2.value == v._1._1.value
         |      ))
         |    }
         |  else{
         |     allOf(Coll(
         |        v._1._2.value == v._1._1.value - v._2._2
         |     ))
         |    }
         |  }}
         |
         |val check = {
         |
         |  if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |
         |    val linkListTokenOutput = OUTPUTS(0)
         |    val linkListElementOutput = OUTPUTS(1)
         |    val maintainerOutput = OUTPUTS(2)
         |
         |    val fee = INPUTS(1).R4[Int].get
         |    val amount = linkListElementOutput.R5[BigInt].get + fee * linkListElementOutput.R5[BigInt].get / 10000
         |
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).id == SELF.id,
         |
         |      linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |      blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |      maintainerOutput.propositionBytes == SELF.propositionBytes,
         |      maintainerOutput.R4[Int].get == INPUTS(0).R4[Int].get,
         |      storeInMaintainer(((INPUTS(1), maintainerOutput),amount)) == true
         |    ))
         |  }
         |  else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // unlock
         |    val maintainerOutput = OUTPUTS(1)
         |    val fee = INPUTS(2).R4[Int].get
         |    val amount = INPUTS(2).R5[BigInt].get + fee * INPUTS(2).R5[BigInt].get / 10000
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val receiver = data.slice(66, data.size)
         |    allOf(Coll(
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).id == SELF.id,
         |
         |      maintainerOutput.tokens(0)._1 == maintainerRepoId,
         |      maintainerOutput.tokens(1)._1 == maintainerNFTToken,
         |
         |      OUTPUTS(2).tokens(0)._1 == maintainerRepoId,
         |
         |      unlock(((INPUTS(2), maintainerOutput),(OUTPUTS(2), amount))) == true,
         |      OUTPUTS(2).propositionBytes == receiver
         |    ))
         |  }
         |  else false
         |}
         |  sigmaProp (check)
         |}""".stripMargin
    val linkListElementScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |
         |      allOf(Coll(
         |        INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |
         |        linkListElementOutput.propositionBytes == SELF.propositionBytes,
         |        linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListElementOutput.tokens(0)._2 == 1,
         |        linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
         |        linkListElementOutput.R5[BigInt].isDefined, // request amount
         |        linkListElementOutput.R6[BigInt].isDefined, // request id
         |        linkListElementOutput.value == minValue,
         |
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // approve
         |      allOf(Coll(
         |        INPUTS(2).tokens(1)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id
         |     ))
         |    }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin

    var boxes = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      boxes.size
    }")
    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBoxId: String) = issueNFTToken(prover, boxes.head, "LUPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBoxId: String) = issueNFTToken(prover, boxes.drop(1).head, "LUPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBoxId: String) = issueToken(prover, boxes.drop(2).head, "LUPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")

    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", 1000000L)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .build(),
      linkListElementScript
    )
    val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
    val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

    lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("linkListTokenRepoId", ErgoId.create(linkListTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("minValue", 1000000L)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      linkListRepoScript
    )

    lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("maintainerRepoId", ErgoId.create(Configs.sWTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val boxFee = boxes.drop(3)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(linkListTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreateLinkListBox:")
    createLinkListBox(ctx, prover, boxFee, linkListRepoContract, linkListTokenBoxId, linkListRepoTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(maintainerTokenBoxId)
          ctx.getCoveringBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString), (1e9 * 1e8).toLong).getBoxes.asScala.toList.head
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    boxes = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList
    val box = boxes.filter(box => box.getTokens.size() == 0 && box.getValue > 2 * Configs.defaultTxFee).head
    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, box, maintainerRepoContract, maintainerTokenBoxId, Configs.sWTokenId)
  }

  def runIBPort(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    val secret = BigInt(Configs.proverSecret, 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()

    val linkListRepoScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(0).tokens(0)._2 - 1,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(0).value - minValue,
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |     }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
         |      val linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(2).tokens(0)._2 + 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(2).value + minValue,
         |
         |        INPUTS(2).propositionBytes == SELF.propositionBytes,
         |        INPUTS(2).id == SELF.id,
         |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
         |       ))
         |     }
         |    else false
         |  }
         |
         |  sigmaProp (check)
         |}""".stripMargin

    val maintainerRepoScript: String =
      s"""{
         |val storeInMaintainer: Boolean = {(v: ((Box, Box), BigInt )) => {
         |    if (v._1._1.tokens.size > 1){
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value,
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2
         |      ))
         |    }
         |    else{
         |       allOf(Coll(
         |          v._1._2.value == v._1._1.value + v._2
         |      ))
         |    }
         |  }}
         |
         |val mint: Boolean = {(v: ((Box, Box), (Box, BigInt))) => {
         |  if (v._1._1.tokens.size > 1){
         |      allOf(Coll(
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2,
         |          v._1._2.value == v._1._1.value
         |      ))
         |    }
         |    else{
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value - v._2._2
         |      ))
         |    }
         |  }}
         |
         |val check = {
         |
         |  if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |
         |    val linkListTokenOutput = OUTPUTS(0)
         |    val linkListElementOutput = OUTPUTS(1)
         |    val maintainerOutput = OUTPUTS(2)
         |
         |    val amount = linkListElementOutput.R5[BigInt].get
         |
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).id == SELF.id,
         |
         |      linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |      blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |      maintainerOutput.propositionBytes == SELF.propositionBytes,
         |      maintainerOutput.R4[Int].get == INPUTS(0).R4[Int].get,
         |      storeInMaintainer(((INPUTS(1), maintainerOutput), amount)) == true
         |    ))
         |  }
         |  else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // Mint
         |    val maintainerOutput = OUTPUTS(1)
         |    val amount = INPUTS(2).R5[BigInt].get
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val receiver = data.slice(66, data.size)
         |    allOf(Coll(
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).id == SELF.id,
         |
         |      maintainerOutput.tokens(0)._1 == maintainerRepoId,
         |      maintainerOutput.tokens(1)._1 == maintainerNFTToken,
         |
         |      OUTPUTS(2).tokens(0)._1 == maintainerRepoId,
         |
         |      mint(((INPUTS(2), maintainerOutput), (OUTPUTS(2), amount))) == true,
         |      OUTPUTS(2).propositionBytes == receiver
         |    ))
         |  }
         |  else false
         |}
         |  sigmaProp (check)
         |}""".stripMargin

    val linkListElementScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListElementOutput = OUTPUTS(1)
         |      val linkListTokenOutput = OUTPUTS(0)
         |
         |      allOf(Coll(
         |       INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |       linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |
         |       linkListElementOutput.propositionBytes == SELF.propositionBytes,
         |       linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
         |       linkListElementOutput.tokens(0)._2 == 1,
         |       linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
         |       linkListElementOutput.R5[BigInt].isDefined, // request amount
         |       linkListElementOutput.R6[BigInt].isDefined, // request id
         |       linkListElementOutput.value == minValue,
         |
         |       OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
         |      allOf(Coll(
         |        INPUTS(2).tokens(1)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id
         |      ))
         |     }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin

    var boxes = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      boxes.size
    }")
    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBoxId: String) = issueNFTToken(prover, boxes.head, "IBPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBoxId: String) = issueNFTToken(prover, boxes.drop(1).head, "IBPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBoxId: String) = issueToken(prover, boxes.drop(2).head, "IBPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")

    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", 1000000L)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .build(),
      linkListElementScript
    )
    val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
    val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

    lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("linkListTokenRepoId", ErgoId.create(linkListTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("minValue", 1000000L)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      linkListRepoScript
    )

    lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("maintainerRepoId", ErgoId.create(Configs.sWTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val boxFee = boxes.drop(3)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(linkListTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreateLinkListBox:")
    createLinkListBox(ctx, prover, boxFee, linkListRepoContract, linkListTokenBoxId, linkListRepoTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(maintainerTokenBoxId)
          ctx.getCoveringBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString), (1e9 * 1e8).toLong).getBoxes.asScala.toList.head
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    boxes = ctx.getCoveringBoxesFor(our, (1e9 * 1e8).toLong).getBoxes.asScala.toList
    val box = boxes.filter(box => box.getTokens.size() == 0 && box.getValue > 2 * Configs.defaultTxFee).head
    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, box, maintainerRepoContract, maintainerTokenBoxId, Configs.gwSWTokenId)

  }

  def run(ctx: BlockchainContext): Unit = {

    // uncomment this section if you want to issue SWTokenId, gwSWTokenId with code

    //    val secret = BigInt(Configs.proverSecret, 16)
    //    val prover = ctx.newProverBuilder()
    //      .withDLogSecret(secret.bigInteger)
    //      .build()
    //    val boxes = ctx.getUnspentBoxesFor(our, 0, 500).asScala.toList
    //      .filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    //    println(boxes.size)
    //    val (a, gwSWTokenBoxId: String) = issueToken(prover, boxes.head, "gwSWToken", "gravity wrap token", 100)
    ////    val (b, sWTokenBoxId: String) = issueToken(prover, boxes.head, "SWToken", "gravity token", 100)
    //
    ////    println(s"SWTokenId: ${b},\n SWTokenBoxId: ${sWTokenBoxId} ")
    //    println(s"gwSWTokenId: ${a},\n gwSWTokenBoxId: ${gwSWTokenBoxId} ")

    println("\n\t\t\tLUPort:")
    runLUPort(ctx, Configs.sigmaTokenRepoTokenId)

    println("\n\t\t\tIBPort:")
    runIBPort(ctx, Configs.ergoTokenRepoTokenId)

  }
}
