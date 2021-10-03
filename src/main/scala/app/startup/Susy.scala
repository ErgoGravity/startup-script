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
import io.circe.syntax._
import java.io.PrintWriter
import java.math.BigInteger
import io.circe.{Json => ciJson}
import scala.collection.JavaConverters._

object Susy {
  val addrEnc = new ErgoAddressEncoder(NetworkType.MAINNET.networkPrefix)
  val secureRandom = new java.security.SecureRandom
  val feeAddress: Address = Address.create(Configs.feeAddress)
  val linkListNFTCount: Int = 1

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

  def issueNFTToken(prover: ErgoProver, box: InputBox, tokenName: String, tokenDescription: String): (String, InputBox) = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {
      val txB = ctx.newTxBuilder()
      val id = box.getId.toString
      val issuingNum = 1L
      val newBox = txB.outBoxBuilder
        .value(Configs.defaultTxFee)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")), ErgoValue.of(Array(1.toByte)))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(feeAddress.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(Configs.defaultTxFee)
        .sendChangeTo(feeAddress.getErgoAddress)
        .build()

      val signed: SignedTransaction = prover.sign(tx)
      new PrintWriter(s"result_susy/${tokenName}_signed.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_susy/${tokenName}_txId.txt") {
        write(txId)
        close()
      }
      println(s"txId: $txId")
      (id, signed.getOutputsToSpend.get(0))
    })
  }

  def issueToken(prover: ErgoProver, box: InputBox, tokenName: String, tokenDescription: String, issuingNum: Long = 1000000L, address: Address = feeAddress): (String, InputBox) = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {

      val txB = ctx.newTxBuilder()
      val id = box.getId.toString
      val newBox = txB.outBoxBuilder
        .value(Configs.defaultTxFee)
        .registers(ErgoValue.of(tokenName.getBytes("utf-8")),
          ErgoValue.of(tokenDescription.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")))
        .tokens(new ErgoToken(id, issuingNum))
        .contract(new ErgoTreeContract(address.getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(box).asJava)
        .outputs(newBox)
        .fee(Configs.defaultTxFee)
        .sendChangeTo(address.getErgoAddress)
        .build()

      val signed: SignedTransaction = prover.sign(tx)
      new PrintWriter(s"result_susy/${tokenName}_signed.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_susy/${tokenName}_txId.txt") {
        write(txId)
        close()
      }
      println(s"txId: $txId")
      (id, signed.getOutputsToSpend.get(0))
    })
  }

  def issueSusyWrappedTokens(ctx: BlockchainContext): Unit = {
    var secret = BigInt(Configs.proverSecret, 16)
    var prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()
    val boxes = ctx.getCoveringBoxesFor(feeAddress, (1e9 * 1e8).toLong).getBoxes.asScala.toList
      .filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(boxes.size)
    val (tonkenId, tonkenBox) = issueToken(prover, boxes.head, "USDN", "susy USDN token", 1000)
    println(s"tonkenId: ${tonkenId},\ntonkenBoxId: ${tonkenBox.getId.toString} ")

    val (gwTokenId, gwTokenBox) = issueToken(prover, boxes.drop(1).head, "gwUSDN", "susy wrapped USDN token", 1000)
    println(s"gwTokenId: ${gwTokenId},\ngwTokenBoxId: ${gwTokenBox.getId.toString} ")
  }

  def randomAddr(): Unit = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {
      val rnd = randBigInt
      println(s"secret: ${rnd.toString(16)}")
      val addr = getProveDlogAddress(rnd, ctx)
      println(s"pk/address: ${addr}")
    })
  }


  def createLinkListBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: List[InputBox], linkListRepoContract: ErgoContract, linkListNFTTokenBox: InputBox, linkListTokenRepoBox: InputBox, nftNumber: Int): String = {

    val txB = ctx.newTxBuilder()

    var boxes = boxFee.filter(box => box.getValue > 100 * Configs.defaultTxFee)

    var ergBox: List[InputBox] = List()
    var total = 0L
    object AllDone extends Exception {}

    val maxValue = Configs.defaultTxFee * (linkListTokenRepoBox.getTokens.get(0).getValue / linkListNFTCount + 4)
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
        boxes = ctx.getCoveringBoxesFor(feeAddress, (1e9 * 1e8).toLong).getBoxes.asScala.toList
          .filter(box => box.getTokens.size() == 0 && box.getValue >= 100 * Configs.defaultTxFee)
      }
    }


    def createLinkListRepo(txB: UnsignedTransactionBuilder, tokenNFTBox: InputBox, tokenBox: InputBox, nftNumber: Int): OutBox = {
      val requestId = BigInt(0)
      txB.outBoxBuilder
        .value(Configs.defaultTxFee * (tokenBox.getTokens.get(0).getValue / linkListNFTCount + 1))
        .tokens(new ErgoToken(tokenNFTBox.getTokens.get(0).getId, 1),
          new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue / linkListNFTCount))
        .registers(ErgoValue.of(requestId.bigInteger),
          ErgoValue.of(linkListNFTCount),
          ErgoValue.of(nftNumber))
        .contract(linkListRepoContract)
        .build()
    }

    def createChangeBoxes(txB: UnsignedTransactionBuilder, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(total - Configs.defaultTxFee * (linkListTokenRepoBox.getTokens.get(0).getValue + 2))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend((Seq(linkListNFTTokenBox, linkListTokenRepoBox) ++ ergBox).asJava)
      .outputs(createLinkListRepo(txB, linkListNFTTokenBox, linkListTokenRepoBox, nftNumber),
        createChangeBoxes(txB, feeAddress))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(feeAddress.getErgoAddress)
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
    val txId = ctx.sendTransaction(signed)
    new PrintWriter(s"result_susy/LinkListRepoBox_tx_${methodName}.txt") {
      write(txId);
      close()
    }
    println(s"txId: $txId")
    signed.getOutputsToSpend.get(0).getId.toString
  }

  def createMaintainerBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, unspentBoxes: List[InputBox], maintainerRepoContract: ErgoContract, maintainerTokenBox: InputBox, tokenId: String): String = {

    var ownerAddress = feeAddress
    val tokenBox = unspentBoxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(tokenId)
    }).head
    val txB = ctx.newTxBuilder()
    var tokenIdValue = 0L
    if (tokenId == Configs.gwTokenId) {
      tokenIdValue = tokenBox.getTokens.get(0).getValue - 1
    } else if (tokenId == Configs.tokenId) {
      ownerAddress = Address.create(Configs.susyTokenAddress)
      tokenIdValue = 1L
    }


    def createMaintainerRepo(txB: UnsignedTransactionBuilder, tokenNFTBox: InputBox, tokenBox: InputBox): OutBox = {
      val fee: Int = 0
      txB.outBoxBuilder
        .value(tokenNFTBox.getValue + Configs.defaultTxFee)
        .tokens(new ErgoToken(tokenNFTBox.getTokens.get(0).getId, 1),
          new ErgoToken(tokenBox.getTokens.get(0).getId, tokenIdValue))
        .registers(ErgoValue.of(fee))
        .contract(maintainerRepoContract)
        .build()
    }

    def createChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - tokenIdValue))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(maintainerTokenBox, tokenBox, boxFee).asJava)
      .outputs(createMaintainerRepo(txB, maintainerTokenBox, tokenBox),
        createChangeBoxes(txB, boxFee, Configs.defaultTxFee, ownerAddress))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(feeAddress.getErgoAddress)
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
    new PrintWriter(s"result_susy/MaintainerBox_txId_${methodName}.txt") {
      write(txId);
      close()
    }
    println(s"txId: $txId")
    signed.getOutputsToSpend.get(0).getId.toString
  }

  def runLUPort(ctx: BlockchainContext, tokenRepoTokenId: String): String = {

    val secret = BigInt(Configs.proverSecret, 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()

    val linkListRepoScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        INPUTS(1).tokens(0)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._2 == INPUTS(0).tokens(1)._2 - 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
         |        linkListTokenOutput.R5[Int].isDefined, // nft count
         |        linkListTokenOutput.R6[Int].isDefined, // nft number
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(0).value - minValue,//TODO : check minvalue
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // approve
         |      val linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(1)._2 == INPUTS(2).tokens(1)._2 + 1,
         |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
         |        linkListTokenOutput.R5[Int].isDefined, // nft count
         |        linkListTokenOutput.R6[Int].isDefined, // nft number
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
         |val storeInMaintainer = {(v: ((Box, Box), (Int, Long) )) => {
         |    val amount = v._2._2 + v._2._1 * v._2._2 / 10000
         |    if (v._1._1.tokens.size > 1){
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value,
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + amount
         |      ))
         |    }
         |    else{
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value + amount
         |      ))
         |    }
         |  }}
         |
         |val unlock: Boolean = {(v: ((Box, Box), (Box, Long))) => {
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
         |val createTransferRequestScenario = {
         |  if(INPUTS(0).tokens(0)._1 == linkListNFTToken && INPUTS(0).R5[Int].isDefined){
         |    val linkListTokenRepo = OUTPUTS(0)
         |    val linkListElement = OUTPUTS(1)
         |    val maintainer = OUTPUTS(2)
         |    val fee = INPUTS(1).R4[Int].get
         |    val amount = OUTPUTS(1).R5[Long].get
         |
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListNFTToken,
         |      INPUTS(0).tokens(1)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).id == SELF.id,
         |
         |      linkListTokenRepo.tokens(0)._1 == linkListNFTToken,
         |      blake2b256(linkListElement.propositionBytes) == linkListElementRepoContractHash,
         |
         |      maintainer.tokens(0)._1 == maintainerNFTToken,
         |      maintainer.tokens(1)._1 == maintainerRepoId,
         |      maintainer.propositionBytes == SELF.propositionBytes,
         |      maintainer.R4[Int].get == INPUTS(1).R4[Int].get,
         |      storeInMaintainer(( (INPUTS(1), OUTPUTS(2)), (fee, amount) ))
         |    ))}
         |   else false
         | }
         | val unlockScenario = {
         |  if(INPUTS(0).tokens(0)._1 == signalTokenNFT && INPUTS(0).R5[Coll[Byte]].isDefined){
         |    // OUTPUTS(0) -> tokenRepo
         |    val maintainer = OUTPUTS(1)
         |    // OUTPUTS(2) -> receiver
         |
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val amount = byteArrayToLong(data.slice(33, 65))
         |    val fee = INPUTS(2).R4[Int].get
         |    val newAmount = amount + fee * amount / 10000
         |
         |    allOf(Coll(
         |
         |      INPUTS(0).tokens(0)._1 == signalTokenNFT,
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).id == SELF.id,
         |
         |      maintainer.tokens(1)._1 == maintainerRepoId,
         |      maintainer.tokens(0)._1 == maintainerNFTToken,
         |      maintainer.propositionBytes == SELF.propositionBytes,
         |      maintainer.R4[Int].get == INPUTS(2).R4[Int].get,
         |
         |      unlock(((INPUTS(2), maintainer), (OUTPUTS(2), newAmount)))
         |      //OUTPUTS(2).propositionBytes == receiver
         |    ))
         |   }
         |   else false
         | }
         |sigmaProp (createTransferRequestScenario || unlockScenario)
         |}""".stripMargin

    val linkListElementScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |
         |      allOf(Coll(
         |        INPUTS(1).tokens(0)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |
         |        linkListElementOutput.propositionBytes == SELF.propositionBytes,
         |        linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
         |        linkListElementOutput.tokens(0)._2 == 1,
         |        linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
         |        linkListElementOutput.R5[Long].isDefined, // request amount
         |        linkListElementOutput.R6[Long].isDefined, // request id
         |        linkListElementOutput.value == minValue,
         |
         |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // approve
         |      allOf(Coll(
         |        INPUTS(2).tokens(0)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id
         |     ))
         |    }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin


    var unspentBoxes = ctx.getCoveringBoxesFor(feeAddress, (1e9 * 1e8).toLong).getBoxes.asScala.toList
    val unspent = unspentBoxes.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      unspent.size
    }")

    var spends = List[InputBox]()
    val idList = Utils.findMempoolBox(feeAddress.getErgoAddress.toString)
    print(idList)
    for (box <- unspent) {
      breakable {
        for (id <- idList) {
          if (id.equals(box.getId.toString)) {
            spends = spends :+ box
            break
          }
        }
      }
    }
    var boxes = unspent.filterNot(spends.toSet)
      .sortWith((s, t) => s.getValue < t.getValue)
    println(s"size: ${
      boxes.size
    }")

    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBox) = issueNFTToken(prover, boxes.head, "LUPort_Linklist_NFT", "Gravity Project: https://gravity.tech/")
    boxes = boxes.drop(1)

    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBox) = issueNFTToken(prover, boxes.drop(1).head, "LUPort_Maintainer_NFT", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBox) = issueToken(prover, boxes.drop(2).head, "LUPort_LinkListTokenRepo", "Gravity Project: https://gravity.tech/")
    boxes = boxes.drop(3)

    val tokens = Map("linkListTokenId" -> linkListTokenId, "maintainerTokenId" -> maintainerTokenId, "linkListRepoTokenId" -> linkListRepoTokenId, "tokenId" -> Configs.tokenId)
    new PrintWriter("result_susy/LUPort_Tokens.txt") {
      write("tokens: {\n")
      tokens.foreach {
        case (k, v) =>
          write("\t" + k + ": " + v + "\n")
      }
      write("}")
      close()
    }

    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", Configs.defaultTxFee)
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
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("minValue", Configs.defaultTxFee)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      linkListRepoScript
    )

    lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("maintainerRepoId", ErgoId.create(Configs.tokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val linkListAddress = Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString
    val maintainerAddress = Configs.addressEncoder.fromProposition(maintainerRepoContract.getErgoTree).get.toString
    println("\n\t\t\tluport linkListAddress:")
    println(linkListAddress)
    println("\n\t\t\tluport maintainerAddress:")
    println(maintainerAddress)
    println("\n\t\t\tluport linkListElementAddress:")
    println(Configs.addressEncoder.fromProposition(linkListElementErgoTee).get.toString)

    breakable {
      while (true) {
        try {
          ctx.getBoxesById(maintainerTokenBox.getId.toString).head
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreating maintainerBox:")
    val maintainerBoxID = createMaintainerBox(ctx, prover, boxes.head, unspentBoxes, maintainerRepoContract, maintainerTokenBox, Configs.tokenId)

    breakable {
      while (true) {
        try {
          ctx.getBoxesById(linkListRepoTokenBox.getId.toString).head
          ctx.getBoxesById(linkListTokenBox.getId.toString).head
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    val boxFee = boxes.drop(6)
    //
    println("\n\t\t\tcreating linkListBox:")
    val lastLinkListBoxId = createLinkListBox(ctx, prover, boxFee, linkListRepoContract, linkListTokenBox, linkListRepoTokenBox, 0)
    lastLinkListBoxId
  }

  def runIBPort(ctx: BlockchainContext, tokenRepoTokenId: String): Unit = {
    val secret = BigInt(Configs.proverSecret, 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()

    val linkListRepoScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListTokenOutput = OUTPUTS(0)
         |      val linkListElementOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        INPUTS(1).tokens(0)._1 == maintainerNFTToken,
         |
         |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(1)._2 == INPUTS(0).tokens(1)._2 - 1,
         |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
         |        linkListTokenOutput.R5[Int].isDefined, // nft count
         |        linkListTokenOutput.R6[Int].isDefined, // nft number
         |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
         |        linkListTokenOutput.value == INPUTS(0).value - minValue,
         |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
         |
         |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
         |      ))
         |     }
         |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // ChangeStatus
         |      val linkListTokenOutput = OUTPUTS(1)
         |      allOf(Coll(
         |        linkListTokenOutput.tokens(1)._2 == INPUTS(2).tokens(1)._2 + 1,
         |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
         |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
         |        linkListTokenOutput.R5[Int].isDefined, // nft count
         |        linkListTokenOutput.R6[Int].isDefined, // nft number
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
         | val storeInMaintainer = {(v: ((Box, Box), (Int, Long) )) => {
         |    if (v._1._1.tokens.size > 1){
         |      allOf(Coll(
         |          v._1._2.value == v._1._1.value,
         |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
         |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2._2
         |      ))
         |    }
         |    else{
         |       allOf(Coll(
         |          v._1._2.value == v._1._1.value + v._2._2
         |      ))
         |    }
         |  }}
         |
         | val mint: Boolean = {(v: ((Box, Box), (Box, Long))) => {
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
         |val createTransferRequestScenario = {
         | if(INPUTS(0).tokens(0)._1 == linkListNFTToken && INPUTS(0).R5[Int].isDefined){
         |    val linkListTokenRepo = OUTPUTS(0)
         |    val linkListElement = OUTPUTS(1)
         |    val maintainer = OUTPUTS(2)
         |    val fee = INPUTS(1).R4[Int].get
         |    val amount = OUTPUTS(1).R5[Long].get
         |
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == linkListNFTToken,
         |      INPUTS(0).tokens(1)._1 == linkListTokenRepoId,
         |      INPUTS(1).propositionBytes == SELF.propositionBytes,
         |      INPUTS(1).id == SELF.id,
         |
         |      linkListTokenRepo.tokens(0)._1 == linkListNFTToken,
         |      blake2b256(linkListElement.propositionBytes) == linkListElementRepoContractHash,
         |
         |      maintainer.tokens(0)._1 == maintainerNFTToken,
         |      maintainer.tokens(1)._1 == maintainerRepoId,
         |      maintainer.propositionBytes == SELF.propositionBytes,
         |      maintainer.R4[Int].get == INPUTS(1).R4[Int].get,
         |      storeInMaintainer(((INPUTS(1), OUTPUTS(2)), (fee, amount)))
         |    ))
         |    }
         |    else false
         |  }
         | val mintScenario = {
         |   if(INPUTS(0).tokens(0)._1 == signalTokenNFT && INPUTS(0).R5[Coll[Byte]].isDefined){
         |
         |    // OUTPUTS(0) -> tokenRepo
         |    val maintainer = OUTPUTS(1)
         |    // OUTPUTS(2) -> receiver
         |    val data = INPUTS(0).R5[Coll[Byte]].get
         |    val amount = byteArrayToLong(data.slice(33, 65))
         |    //data.slice(66, data.size) -> receiver address
         |
         |    allOf(Coll(
         |      INPUTS(0).tokens(0)._1 == signalTokenNFT,
         |      INPUTS(2).propositionBytes == SELF.propositionBytes,
         |      INPUTS(2).id == SELF.id,
         |
         |      maintainer.tokens(0)._1 == maintainerNFTToken,
         |      maintainer.tokens(0)._2 == 1,
         |      maintainer.tokens(1)._1 == maintainerRepoId,
         |      maintainer.propositionBytes == SELF.propositionBytes,
         |      maintainer.R4[Int].get == INPUTS(2).R4[Int].get,
         |
         |      mint(((INPUTS(2), maintainer), (OUTPUTS(2), amount) ))
         |      //OUTPUTS(2).propositionBytes == receiver
         |    ))
         |   }
         |    else false
         |  }
         |sigmaProp (createTransferRequestScenario || mintScenario)
         |}""".stripMargin

    val linkListElementScript: String =
      s"""{
         |  val check = {
         |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
         |      val linkListElementOutput = OUTPUTS(1)
         |      val linkListTokenOutput = OUTPUTS(0)
         |
         |      allOf(Coll(
         |       INPUTS(1).tokens(0)._1 == maintainerNFTToken,
         |
         |       linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
         |
         |       linkListElementOutput.propositionBytes == SELF.propositionBytes,
         |       linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
         |       linkListElementOutput.tokens(0)._2 == 1,
         |       linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
         |       linkListElementOutput.R5[Long].isDefined, // request amount
         |       linkListElementOutput.R6[BigInt].isDefined, // request id
         |       linkListElementOutput.value == minValue,
         |
         |       OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
         |      ))
         |    }
         |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // ChangeStatus
         |      allOf(Coll(
         |        INPUTS(2).tokens(0)._1 == linkListNFTToken,
         |        INPUTS(3).propositionBytes == SELF.propositionBytes,
         |        INPUTS(3).id == SELF.id
         |      ))
         |     }
         |    else false
         |  }
         |  sigmaProp (check)
         |}""".stripMargin

    var unspentBoxes = ctx.getCoveringBoxesFor(feeAddress, (1e9 * 1e8).toLong).getBoxes.asScala.toList
    val unspent = unspentBoxes.filter(box => box.getTokens.size == 0 && box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      unspent.size
    }")

    var spends = List[InputBox]()
    val idList = Utils.findMempoolBox(feeAddress.getErgoAddress.toString)
    print(idList)
    for (box <- unspent) {
      breakable {
        for (id <- idList) {
          if (id.equals(box.getId.toString)) {
            spends = spends :+ box
            break
          }
        }
      }
    }
    var boxes = unspent.filterNot(spends.toSet)
    println("\n\t\t\tissuing linkListTokenId:")
    val (linkListTokenId, linkListTokenBox) = issueNFTToken(prover, boxes.head, "IBPort_Linklist_NFT_", "Gravity Project: https://gravity.tech/")
    boxes = boxes.drop(1)

    println("\n\t\t\tissuing maintainerTokenId:")
    val (maintainerTokenId, maintainerTokenBox) = issueNFTToken(prover, boxes.drop(1).head, "IBPort_Maintainer_NFT_", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing linkListTokenRepoTokenId:")
    val (linkListRepoTokenId, linkListRepoTokenBox) = issueToken(prover, boxes.drop(2).head, "IBPort_LinkListTokenRepo_", "Gravity Project: https://gravity.tech/")
    boxes = boxes.drop(3)
    val tokens = Map("linkListTokenId" -> linkListTokenId, "maintainerTokenId" -> maintainerTokenId, "linkListRepoTokenId" -> linkListRepoTokenId, "gwTokenId" -> Configs.gwTokenId)
    new PrintWriter("result_susy/IBPort_Tokens.txt") {
      write("tokens: {\n")
      tokens.foreach {
        case (k, v) =>
          write("\t" + k + ": " + v + "\n")
      }
      write("}")
      close()
    }


    lazy val linkListElementContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("minValue", Configs.defaultTxFee)
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
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("minValue", Configs.defaultTxFee)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      linkListRepoScript
    )

    lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("maintainerNFTToken", ErgoId.create(maintainerTokenId).getBytes)
        .item("maintainerRepoId", ErgoId.create(Configs.gwTokenId).getBytes)
        .item("linkListTokenRepoId", ErgoId.create(linkListRepoTokenId).getBytes)
        .item("linkListNFTToken", ErgoId.create(linkListTokenId).getBytes)
        .item("signalTokenNFT", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("linkListElementRepoContractHash", linkListElementHash)
        .build(),
      maintainerRepoScript
    )

    val linkListAddress = Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString
    val maintainerAddress = Configs.addressEncoder.fromProposition(maintainerRepoContract.getErgoTree).get.toString
    println("\n\t\t\tibport linkListAddress:")
    println(linkListAddress)
    println("\n\t\t\tibport maintainerAddress:")
    println(maintainerAddress)
    println("\n\t\t\tibport linkListElementAddress:")
    println(Configs.addressEncoder.fromProposition(linkListElementErgoTee).get.toString)


    breakable {
      while (true) {
        try {
          ctx.getBoxesById(maintainerTokenBox.getId.toString).head
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreating maintainerBox:")
    val maintainerBoxID = createMaintainerBox(ctx, prover, boxes.head, unspentBoxes, maintainerRepoContract, maintainerTokenBox, Configs.gwTokenId)

    breakable {
      while (true) {
        try {
          ctx.getBoxesById(linkListRepoTokenBox.getId.toString).head
          ctx.getBoxesById(linkListTokenBox.getId.toString).head
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    val boxFee = boxes.drop(6)

    println("\n\t\t\tcreating linkListBox:")
    var boxId = createLinkListBox(ctx, prover, boxFee, linkListRepoContract, linkListTokenBox, linkListRepoTokenBox, 0)

  }

  def run(ctx: BlockchainContext): Unit = {

    // to make new address and secret use this
    //    randomAddr()
    // If you want to issue SUSY tokens with code, you can uncomment the below line and edit the function as you wish.
    //          issueSusyWrappedTokens(ctx)

    println("\n\t\t\tLUPort:")
    val LUPortLinkListBoxBoxId = runLUPort(ctx, Configs.tokenRepoTokenId)

    breakable {
      while (true) {
        try {
          ctx.getBoxesById(LUPortLinkListBoxBoxId).head
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tIBPort:")
    runIBPort(ctx, Configs.tokenRepoTokenId)

  }
}
