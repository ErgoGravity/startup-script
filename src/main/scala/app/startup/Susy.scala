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
      new PrintWriter(s"result_gateway/${tokenName}_signed_${methodName}.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_gateway/${tokenName}_txId_${methodName}.txt") {
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
      new PrintWriter(s"result_gateway/${tokenName}_signed_${methodName}.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_gateway/${tokenName}_txId_${methodName}.txt") {
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

  def createLinkListBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, linkListRepoContract: ErgoContract, linkListTokenBoxId: String, linkListTokenRepoBoxId: String): Unit = {
    val tokenNFTBox = ctx.getBoxesById(linkListTokenBoxId).head
    val tokenBox = ctx.getBoxesById(linkListTokenRepoBoxId).head
    val txB = ctx.newTxBuilder()

    def createLinkListRepo(txB: UnsignedTransactionBuilder, tokenNFTBox: InputBox, tokenBox: InputBox, linkListRepoContract: ErgoContract): OutBox = {
      txB.outBoxBuilder
        .value(tokenNFTBox.getValue + tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1),
          new ErgoToken(tokenNFTBox.getTokens.get(0).getId, 1))
        .contract(linkListRepoContract)
        .build()
    }

    def createChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue -1))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(tokenNFTBox, tokenBox, boxFee).asJava)
      .outputs(createLinkListRepo(txB, tokenNFTBox, tokenBox, linkListRepoContract),
        createChangeBoxes(txB, boxFee, Configs.defaultTxFee, our))
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
    new PrintWriter(s"LinkListRepoBox_signed_${methodName}.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter(s"LinkListRepoBox_tx_${methodName}.txt") {
      write(txId);
      close()
    }
    println(txId)
  }

  def createLinkListElementBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, linkListElementContract: ErgoContract, linkListBox: InputBox): Unit = {
    val txB = ctx.newTxBuilder()

    def createLinkListElement(txB: UnsignedTransactionBuilder, linkListRepo: InputBox, linkListElementContract: ErgoContract): OutBox = {
      val receiverAddress = ErgoValue.of(Address.create("").toString.getBytes())
      val zero = 0
      val amount: BigInt = ErgoValue.of(BigInt("0", 16))
      val reqId: BigInt = ErgoValue.of(BigInt(zero))

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
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue -1))
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
    new PrintWriter(s"result_gateway/MaintainerBox_signed_${methodName}.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter(s"result_gateway/MaintainerBox_txId_${methodName}.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def createMaintainerBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, maintainerRepoContract: ErgoContract, maintainerTokenBoxId: String, TokenId: String, linkListBox: InputBox, linkListElementBox: InputBox): Unit = {
    val tokenNFTBox = ctx.getBoxesById(maintainerTokenBoxId).head
    println("1")
    val tokenBoxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => {
      box.getTokens.size() > 0
    })
    println("2")
    val tokenBox = tokenBoxes.filter(box => {
      box.getTokens.get(0).getId.toString.equals(TokenId)
    }).head
    //    val tokenBox = ctx.getBoxesById(maintainerTokenRepoBoxId).head
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
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue -1))
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
    new PrintWriter(s"result_gateway/MaintainerBox_signed_${methodName}.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter(s"result_gateway/MaintainerBox_txId_${methodName}.txt") {
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
    val a = prover
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
         |val storeInMaintainer: Boolean = {(v: ((Box, Box), BigInt) ) => {
         |    if (v._1._1.tokens.size > 1){
         |          v._1._2.value == v._1._1.value &&
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2
         |    }
         |    else{
         |          v._1._2.value == v._1._1.value + v._2
         |    }
         |  }
         |
         |val unlock: Boolean = {(v: ( (Box, Box) , (Box, BigInt) ) ) => {
         |  if (v._1._1.tokens.size > 1){
         |      v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
         |      v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2 &&
         |      v._1._2.value == v._1._1.value
         |    }
         |    else{
         |       v._1._2.value == v._1._1.value - v._2._2
         |    }
         |  }
         |
         |val check = {
         |
         |  if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |    val fee = INPUTS(1).R4[Int].get
         |    val amount = linkListElementOutput.R5[BigInt].get + fee * linkListElementOutput.R5[BigInt].get / 10000
         |
         |    val linkListTokenOutput = OUTPUTS(0)
         |    val linkListElementOutput = OUTPUTS(1)
         |    val maintainerOutput = OUTPUTS(2)
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).id == SELF.id
         |
         |      linkListTokenOutput.tokens(1)._1 == linkListNFTToken
         |      blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |      maintainerOutput.propositionBytes == SELF.propositionBytes,
         |      maintainerOutput.R4[Int].get == INPUTS(0).R4[Int].get,
         |      storeInMaintainer( ( (INPUTS(1), maintainerOutput), amount) ) == true
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
         |      unlock( (INPUTS(2), maintainerOutput), (OUTPUTS(2), amount) ) == true,
         |      OUTPUTS(2).propositionBytes == receiver
         |    ))
         |  }
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
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken,
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // approve
         |      allOf(Coll(
         |        INPUTS(2).tokens(1)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id,
         |     ))
         |    }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin

    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      boxes.size
    }")
    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBoxId: String) = issueNFTToken(prover, boxes.head, "LUPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBoxId: String) = issueNFTToken(prover, boxes.drop(1).head, "LUPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    //    println("\n\t\t\tissuing maintainerTokenRepoTokenId:")
    //    val (maintainerRepoTokenId, maintainerRepoTokenBoxId: String) = issueToken(prover, boxes.drop(2).head, "LUPort_MaintainerTokenRepo", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBoxId: String) = issueToken(prover, boxes.drop(3).head, "LUPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")

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
        .item("maintainerRepoId", ErgoId.create(Configs.SWTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val boxFee = boxes.drop(4)

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
    createLinkListBox(ctx, prover, boxFee.drop(1).head, linkListRepoContract, linkListTokenBoxId, linkListRepoTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(maintainerTokenBoxId)
          ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString)).asScala.toList.head
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    println("\n\t\t\tcreateMaintainerBox:")
    var linkListRepoBox = ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString)).asScala.toList.head
    createLinkListElementBox(ctx, prover, boxFee.head, linkListElementContract, linkListRepoBox)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(maintainerTokenBoxId)
          ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString)).asScala.toList.head
          ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListElementContract.getErgoTree).get.toString)).asScala.toList.head
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    linkListRepoBox =  ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString)).asScala.toList.head
    val linkListElementBox = ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(linkListElementContract.getErgoTree).get.toString)).asScala.toList.head
    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, boxFee.head, maintainerRepoContract, maintainerTokenBoxId, Configs.SWTokenId, linkListRepoBox, linkListElementBox)


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
         |      linkListTokenOutput = OUTPUTS(0)
         |      linkListElementOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(0).tokens(0)._2 - 1,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(0).value - minValue,
         |
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |     }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
         |      linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(2).tokens(0)._2 + 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(2).value + minValue,
         |
         |        INPUTS(2).propositionBytes == SELF.propositionBytes,
         |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
         |     }
         |    else false
         |  }
         |
         |  sigmaProp (check)
         |}""".stripMargin

    val maintainerRepoScript: String =
      s"""{
         |val storeInMaintainer: Boolean = {(v: ( (Box, Box), BigInt ) ) => {
         |    if (v._1._1.tokens.size > 1){
         |          v._1._2.value == v._1._1.value &&
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2 &&
         |    }
         |    else{
         |          v._1._2.value == v._1._1.value + v._2
         |    }
         |  }
         |
         |val mint: Boolean = {(v: ( (Box, Box) , (Box, BigInt) ) ) => {
         |  if (v._1._1.tokens.size > 1){
         |      v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
         |      v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2 &&
         |      v._1._2.value == v._1._1.value
         |    }
         |    else{
         |       v._1._2.value == v._1._1.value - v._2._2
         |    }
         |  }
         |val check = {
         |
         |  if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |    linkListTokenOutput = OUTPUTS(0)
         |    linkListElementOutput = OUTPUTS(1)
         |    maintainerOutput = OUTPUTS(2)
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
         |      storeInMaintainer( ( (INPUTS(1), maintainerOutput), amount) ) == true
         |  }
         |  else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // Mint
         |    maintainerOutput = OUTPUTS(1)
         |    val amount = INPUTS(2).R5[BigInt].get
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val receiver = data.slice(66, data.size)
         |    allOf(Coll(
         |      blake2b256(INPUTS(0).propositionBytes) == signalRepoContractHash,
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).id == SELF.id,
         |
         |      maintainerOutput.tokens(0)._1 == maintainerRepoId,
         |      maintainerOutput.tokens(1)._1 == maintainerNFTToken,
         |
         |      mint( (INPUTS(2), maintainerOutput), (OUTPUTS(2), amount) ) == true,
         |      OUTPUTS(2).propositionBytes == receiver
         |  }
         |}
         |  sigmaProp (check)
         |}""".stripMargin

    val linkListElementScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
         |      linkListElementOutput = OUTPUTS(1)
         |      linkListTokenOutput = OUTPUTS(0)
         |      allOf(Coll(
         |       INPUTS(1).tokens(1)._1 == maintainerNFTToken,
         |
         |       linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |       linkListElementOutput.propositionBytes == SELF.propositionBytes,
         |       linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
         |       linkListElementOutput.tokens(0)._2 == 1,
         |       linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
         |       linkListElementOutput.R5[BigInt].isDefined, // request amount
         |       linkListElementOutput.value == minValue,
         |
         |       OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |     }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
         |      allOf(Coll(
         |        INPUTS(2).tokens(1)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id
         |      ))
         |     }
         |    else false
         |  }
         |
         |  sigmaProp (check)
         |}""".stripMargin


    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      boxes.size
    }")

    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBoxId: String) = issueNFTToken(prover, boxes.head, "IBPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBoxId: String) = issueNFTToken(prover, boxes.drop(1).head, "IBPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")

    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBoxId: String) = issueToken(prover, boxes.drop(3).head, "IBPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")


    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", 1000000L)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .build(),
      linkListElementScript
    )
    val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
    val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

    lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
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
        .item("maintainerRepoId", ErgoId.create(Configs.gwSWTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val boxFee = boxes.drop(4)
    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(maintainerTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, boxFee.head, maintainerRepoContract, maintainerTokenBoxId, Configs.gwSWTokenId)

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
    createLinkListBox(ctx, prover, boxFee.drop(1).head, linkListRepoContract, linkListTokenBoxId, linkListRepoTokenBoxId)
  }

  def run(ctx: BlockchainContext): Unit = {
    //    val secret = BigInt(Configs.proverSecret, 16)
    //    val prover = ctx.newProverBuilder()
    //      .withDLogSecret(secret.bigInteger)
    //      .build()
    //    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList
    //      .filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    //    println(boxes.size)
    //    val (a, gwSWTokenBoxId: String) = issueToken(prover, boxes.head, "gwSWToken", "gravity wrap token", 100)
    //    val (b, sWTokenBoxId: String) = issueToken(prover, boxes.drop(1).head, "SWToken", "gravity token", 100)
    //
    //    println(s"SWTokenId: ${b},\n SWTokenBoxId: ${sWTokenBoxId} ")
    //    println(s"gwSWTokenId: ${a},\n gwSWTokenBoxId: ${gwSWTokenBoxId} ")


    println("\n\t\t\tLUPort:")
    runLUPort(ctx, Configs.sigmaTokenRepoTokenId)

    println("\n\t\t\tIBPort:")
    runIBPort(ctx, Configs.ergoTokenRepoTokenId)

  }
}
