package proxy.startup

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
import Gateway._

import java.io.PrintWriter
import java.math.BigInteger
import scala.collection.JavaConverters._

object Susy {
  val conf: ErgoToolConfig = ErgoToolConfig.load("test.conf")
  val nodeConf: ErgoNodeConfig = conf.getNode
  val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConf)
  val addrEnc = new ErgoAddressEncoder(NetworkType.MAINNET.networkPrefix)
  val secureRandom = new java.security.SecureRandom
  val our = Address.create("9h6odKstXL1ExJTaPWdrk6d3CVhAJodeFwAnNWKQAVucywRyqrk")

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

  def issueNFTToken(prover: ErgoProver, boxes: List[InputBox], tokenName: String, tokenDescription: String): String = {
    ergoClient.execute((ctx: BlockchainContext) => {
      val feeAmount = 1000000L


      val txB = ctx.newTxBuilder()
      val box = boxes.filter(box => box.getValue > 1000000L).head
      val id = box.getId.toString
      val issuingNum = 1L
      val newBox = txB.outBoxBuilder
        .value(1000000L)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")), ErgoValue.of(Array(1.toByte)))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(our.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(feeAmount)
        .sendChangeTo(our.getErgoAddress)
        .build()

      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      println(ctx.sendTransaction(signed))
      id
    })
    //    tokenId
  }

  def issueToken(prover: ErgoProver, boxes: List[InputBox], tokenName: String, tokenDescription: String): String = {
    ergoClient.execute((ctx: BlockchainContext) => {
      val feeAmount = 1000000L

      val txB = ctx.newTxBuilder()
      val box = boxes.filter(box => box.getValue > 1000000L).head
      val id = box.getId.toString
      val issuingNum = 1000000L
      val newBox = txB.outBoxBuilder
        .value(1000000L)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")), ErgoValue.of(Array(2.toByte)))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(our.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(feeAmount)
        .sendChangeTo(our.getErgoAddress)
        .build()

      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      println(ctx.sendTransaction(signed))
      id
    })
    //    tokenId
  }

  def randomAddr(): Unit = {
    ergoClient.execute((ctx: BlockchainContext) => {
      val rnd = randBigInt
      println(s"secret: ${rnd.toString(16)}")
      val addr = getProveDlogAddress(rnd, ctx)
      println(s"pk/address: ${addr}")
    })
  }

  def createMaintainerBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], maintainerRepoContract: ErgoContract, maintainerTokenId: String, maintainerRepoTokenId: String): Unit = {
    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(maintainerRepoTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L
    val addressTokenRepo = Address.create(addrEnc.fromProposition(maintainerRepoContract.getErgoTree).get.toString)

    def CreateMaintainerBox(txB: UnsignedTransactionBuilder, numToken: Long, tokenBox: InputBox, addressTokenRepo: Address) = {
      txB.outBoxBuilder
        .value(1000000L * numToken)
        .registers(ErgoValue.of(2), ErgoValue.of(0))
        .tokens(new ErgoToken(maintainerTokenId, numToken), new ErgoToken(tokenBox.getTokens.get(0).getId, numToken))
        .contract(new ErgoTreeContract(addressTokenRepo.getErgoAddress.script))
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, tokenBox: InputBox, numToken: Long, numTokenBox: Int, feeAmount: Long, ownerAddress: Address): Seq[OutBox] = {
      val changeTokenBox = txB.outBoxBuilder
        .value(1000000L)
        .registers(ErgoValue.of(2), ErgoValue.of(0))
        .tokens(new ErgoToken(maintainerTokenId, numToken),
          new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - (numToken * numTokenBox)))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      val changeFeeBox = txB.outBoxBuilder
        .value(inputFeeBox.getValue - (1000000L * numToken * numTokenBox) - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      Seq(changeTokenBox, changeFeeBox)
    }

    val numTokenBox = 10
    var outboxes: Seq[OutBox] = List.range(0, numTokenBox).map(x => CreateMaintainerBox(txB, 100, tokenBox, addressTokenRepo))
    outboxes = outboxes ++ CreateChangeBoxes(txB, boxFee, tokenBox, 100, numTokenBox, feeAmount, our)
    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(outboxes: _*)
      .fee(feeAmount)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("MaintainerRepoBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("MaintainerRepoBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)
  }

  def createLinkListRepoBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], linkListRepoContract: ErgoContract, linkListTokenId: String, linkListRepoTokenId: String): Unit = {
    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(linkListRepoTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L
    val addressTokenRepo = Address.create(addrEnc.fromProposition(linkListRepoContract.getErgoTree).get.toString)

    def CreateMaintainerBox(txB: UnsignedTransactionBuilder, numToken: Long, tokenBox: InputBox, addressTokenRepo: Address) = {
      txB.outBoxBuilder
        .value(1000000L * numToken)
        .tokens(new ErgoToken(linkListTokenId, numToken), new ErgoToken(tokenBox.getTokens.get(0).getId, numToken))
        .contract(new ErgoTreeContract(addressTokenRepo.getErgoAddress.script))
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, tokenBox: InputBox, numToken: Long, numTokenBox: Int, feeAmount: Long, ownerAddress: Address): Seq[OutBox] = {
      val changeTokenBox = txB.outBoxBuilder
        .value(1000000L)
        .tokens(new ErgoToken(linkListTokenId, numToken),
          new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - (numToken * numTokenBox)))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      val changeFeeBox = txB.outBoxBuilder
        .value(inputFeeBox.getValue - (1000000L * numToken * numTokenBox) - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      Seq(changeTokenBox, changeFeeBox)
    }

    val numTokenBox = 10
    var outboxes: Seq[OutBox] = List.range(0, numTokenBox).map(x => CreateMaintainerBox(txB, 100, tokenBox, addressTokenRepo))
    outboxes = outboxes ++ CreateChangeBoxes(txB, boxFee, tokenBox, 100, numTokenBox, feeAmount, our)
    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(outboxes: _*)
      .fee(feeAmount)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("LinkListRepoBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("LinkListRepoBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)
  }

  def runLUPort(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    val secret = BigInt("a73febe82f334157832ea12ed92e0a4969bb52534e2cc03daec6b94bc0c13cd6", 16)
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
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT  ){ // approve
         |      linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(0)._2 == INPUTS(2).tokens(0)._2 + 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(2).value + minValue,
         |
         |        INPUTS(2).propositionBytes == SELF.propositionBytes,
         |        INPUTS(2).ID == SELF.ID,
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
         |    val fee = INPUTS(1).R4[Int].get,
         |    val amount = linkListElementOutput.R5[BigInt].get + fee * linkListElementOutput.R5[BigInt].get / 10000,
         |
         |    linkListTokenOutput = OUTPUTS(0)
         |    linkListElementOutput = OUTPUTS(1)
         |    maintainerOutput = OUTPUTS(2)
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).ID == SELF.ID
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
         |    maintainerOutput = OUTPUTS(1)
         |    val fee = INPUTS(2).R4[Int].get,
         |    val amount = INPUTS(2).R5[BigInt].get + fee * INPUTS(2).R5[BigInt].get / 10000,
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val receiver = data.slice(66, data.size)
         |    allOf(Coll(
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).ID == SELF.ID,
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
         |      linkListTokenOutput = OUTPUTS(0),
         |      linkListElementOutput = OUTPUTS(1),
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
         |        INPUTS(3).ID == SELF.ID,
         |     ))
         |    }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin

    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList

    println("\n\t\t\tissuing linkListTokenId:")
    val linkListTokenId: String = issueNFTToken(prover, boxes, "LUPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val maintainerTokenId: String = issueNFTToken(prover, boxes, "LUPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenRepoTokenId:")
    val maintainerRepoTokenId: String = issueToken(prover, boxes, "LUPort_MaintainerTokenRepo", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val linkListRepoTokenId: String = issueToken(prover, boxes, "LUPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")


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
        .item("maintainerRepoId", ErgoId.create(maintainerRepoTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, boxes, maintainerRepoContract, maintainerTokenId, maintainerRepoTokenId)
    println("\n\t\t\tcreateLinkListReboBox:")
    createLinkListRepoBox(ctx, prover, boxes, linkListRepoContract, linkListTokenId, linkListRepoTokenId)

  }

  def runIBPort(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    val secret = BigInt("a73febe82f334157832ea12ed92e0a4969bb52534e2cc03daec6b94bc0c13cd6", 16)
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


    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList

    println("\n\t\t\tissuing linkListTokenId:")
    val linkListTokenId: String = issueNFTToken(prover, boxes, "IBPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenId:")
    val maintainerTokenId: String = issueNFTToken(prover, boxes, "IBPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing maintainerTokenRepoTokenId:")
    val maintainerRepoTokenId: String = issueToken(prover, boxes, "IBPort_MaintainerTokenRepo", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val linkListRepoTokenId: String = issueToken(prover, boxes, "IBPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")


    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", 1000000L)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListTokenId).getBytes)
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
        .item("maintainerRepoId", ErgoId.create(maintainerRepoTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    println("\n\t\t\tcreateMaintainerBox:")
    createMaintainerBox(ctx, prover, boxes, maintainerRepoContract, maintainerTokenId, maintainerRepoTokenId)
    println("\n\t\t\tcreateLinkListRepoBox:")
    createLinkListRepoBox(ctx, prover, boxes, linkListRepoContract, linkListTokenId, linkListRepoTokenId)

  }

  def run(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    println("\n\t\t\tLUPort:")
    runLUPort(ctx, tokenRepoTokenId)

    println("\n\t\t\tIBPort:")
    runIBPort(ctx, tokenRepoTokenId)

  }
}
