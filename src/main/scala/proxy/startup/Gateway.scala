package proxy.startup

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit._
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.sigma.GroupElement

import java.io.PrintWriter
import java.math.BigInteger
import scala.collection.JavaConverters._

object Gateway {
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

  def createTokenRepoBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], tokenRepoContract: ErgoContract, tokenRepoTokenId: String): Unit = {
    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(tokenRepoTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L
    val addressTokenRepo = Address.create(addrEnc.fromProposition(tokenRepoContract.getErgoTree).get.toString)

    def CreateTokenBox(txB: UnsignedTransactionBuilder, numToken: Long, tokenBox: InputBox, addressTokenRepo: Address) = {
      txB.outBoxBuilder
        .value(1000000L * numToken)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, numToken))
        .contract(new ErgoTreeContract(addressTokenRepo.getErgoAddress.script))
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, tokenBox: InputBox, numToken: Long, numTokenBox: Int, feeAmount: Long, ownerAddress: Address): Seq[OutBox] = {
      val changeTokenBox = txB.outBoxBuilder
        .value(1000000L)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - (numToken * numTokenBox)))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      val changeFeeBox = txB.outBoxBuilder
        .value(inputFeeBox.getValue - (1000000L * numToken * numTokenBox) - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      Seq(changeTokenBox, changeFeeBox)
    }

    val numTokenBox = 10
    var outboxes: Seq[OutBox] = List.range(0, numTokenBox).map(x => CreateTokenBox(txB, 100, tokenBox, addressTokenRepo))
    outboxes = outboxes ++ CreateChangeBoxes(txB, boxFee, tokenBox, 100L, numTokenBox, feeAmount, our)
    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(outboxes: _*)
      .fee(feeAmount)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("TokenRepoBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("TokenRepoBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)
  }

  def createGravityBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], gravityContract: ErgoContract, gravityTokenId: String): Unit = {

    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(gravityTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L

    val consuls = Seq("9hNZa6tvbk5cjrLuT2hUTFJ8wG8LwaLfWc3LfVg1MLfPp83C5cT", "9emAEs3bGNCyTDSm85QuMbRqE9Nj6zktBPtjfpd8ADfZJ7NyHRN", "9ganzk7M74dSNiLDzNbUvVkKtnn6ACjxX5qNdFM5F7DtcHP97C6", "9eqwvMw7jneoXWXrSMbCJy7KehVDxR6HWhSfU8UFyvdNNXvAWSZ", "9gK1UVAZEfgsGCZeYsTzqfdfr9mPAvxyTFnbr33kWwkp8Aune8E")
    val consulsPri = Seq("1908d2a7738ebd7cdb9f41c55f1a3e17f6df442001e4450d4b75bb02fd2330f2", "4d90d298dce8c0da07287fcca1714c9c65f934459173793c0a56210cb00d4676", "9aba76c8d4f2240822de6c48a30da9e002ef7c051954ff3f148e9689903df1d3", "2f5827bfadbc5689c789402cb631b7b20006b0c0965ce873711f9f1654c68bc", "c017710a52c74015929e5e046752ea401baff2e0b513636dfb9d9de1bcae78be")
    val consulsPrivateKey = consulsPri.map(BigInt(_, 16))


    def createGravity(txB: UnsignedTransactionBuilder, tokenBox: InputBox, gravityContract: ErgoContract, consuls: Seq[String], consulsPrivateKey: Seq[BigInt]): OutBox = {
      val bftValue = ErgoValue.of(3)
      val consulsAddress = consuls.map(Address.create(_).getPublicKeyGE.getEncoded)
      val msg = consulsAddress.fold(JavaHelpers.collFrom("".getBytes))(_.append(_))
      val signs = consulsPrivateKey.map(sign(msg.toArray, _))
      val consulsValue = ErgoValue.of(IndexedSeq(consulsAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))

      val signs_a = ErgoValue.of(signs.map(sign => sign._1).toArray, ErgoType.groupElementType)
      val signs_z = ErgoValue.of(signs.map(sign => sign._2).toArray, ErgoType.bigIntType)
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(bftValue, consulsValue, signs_a, signs_z)
        .contract(gravityContract)
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(createGravity(txB, tokenBox, gravityContract, consuls, consulsPrivateKey), CreateChangeBoxes(txB, boxFee, feeAmount, our))
      .fee(feeAmount)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("GravityBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("GravityBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def changeGravityBox(ctx: BlockchainContext, prover: ErgoProver, boxFeee: List[InputBox], gravityContract: ErgoContract, gravityTokenId: String): Unit = {

    val gravityBoxes = ctx.getUnspentBoxesFor(Address.create(addrEnc.fromProposition(gravityContract.getErgoTree).get.toString)).asScala.toList
    val gravityBox = gravityBoxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(gravityTokenId)
    }).head

    val boxFee = boxFeee.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L

    val consuls = Seq("9hNZa6tvbk5cjrLuT2hUTFJ8wG8LwaLfWc3LfVg1MLfPp83C5cT", "9emAEs3bGNCyTDSm85QuMbRqE9Nj6zktBPtjfpd8ADfZJ7NyHRN", "9ganzk7M74dSNiLDzNbUvVkKtnn6ACjxX5qNdFM5F7DtcHP97C6", "9eqwvMw7jneoXWXrSMbCJy7KehVDxR6HWhSfU8UFyvdNNXvAWSZ", "9gK1UVAZEfgsGCZeYsTzqfdfr9mPAvxyTFnbr33kWwkp8Aune8E")
    val newConsuls = Seq("9hsJjcdVHjisG6eK5LUZELpNNEnWBehKFUVS1iJtNjfuciSE462", "9emAEs3bGNCyTDSm85QuMbRqE9Nj6zktBPtjfpd8ADfZJ7NyHRN", "9ganzk7M74dSNiLDzNbUvVkKtnn6ACjxX5qNdFM5F7DtcHP97C6", "9eqwvMw7jneoXWXrSMbCJy7KehVDxR6HWhSfU8UFyvdNNXvAWSZ", "9gK1UVAZEfgsGCZeYsTzqfdfr9mPAvxyTFnbr33kWwkp8Aune8E")
    val consulsPri = Seq("446e075c46a76beeb4c4a11365e20561faf56162be8d4f3e0d0cf18e82d38b95", "0", "0", "2f5827bfadbc5689c789402cb631b7b20006b0c0965ce873711f9f1654c68bc", "c017710a52c74015929e5e046752ea401baff2e0b513636dfb9d9de1bcae78be")
    val consulsPrivateKey = consulsPri.map(BigInt(_, 16))


    def createGravity(txB: UnsignedTransactionBuilder, tokenBox: InputBox, gravityContract: ErgoContract, consuls: Seq[String], consulsPrivateKey: Seq[BigInt]): OutBox = {
      val bftValue = ErgoValue.of(3)
      val consulsAddress = consuls.map(Address.create(_).getPublicKeyGE.getEncoded)
      val msg = consulsAddress.fold(JavaHelpers.collFrom("".getBytes))(_.append(_))
      val signs = consulsPrivateKey.map(sign(msg.toArray, _))
      val consulsValue = ErgoValue.of(IndexedSeq(consulsAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))

      val signs_a = ErgoValue.of(signs.map(sign => sign._1).toArray, ErgoType.groupElementType)
      val signs_z = ErgoValue.of(signs.map(sign => sign._2).toArray, ErgoType.bigIntType)
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(bftValue, consulsValue, signs_a, signs_z)
        .contract(gravityContract)
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(gravityBox, boxFee).asJava)
      .outputs(createGravity(txB, gravityBox, gravityContract, newConsuls, consulsPrivateKey), CreateChangeBoxes(txB, boxFee, feeAmount, our))
      .fee(feeAmount)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("ChangeGravityBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    //    val txId = ctx.sendTransaction(signed)
    //    new PrintWriter("ChangeGravityBox_tx.txt") { write(txId); close() }
    //    println(txId)

  }

  def createOracleBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], gravityBox: InputBox, oracleContract: ErgoContract, oracleTokenId: String): Unit = {

    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(oracleTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L

    val oracles = Seq("9gM6oXmjA63KKKiLkXhKB2wTD7zugaMJhjaXx6cKvt8kJjiu5xx", "9gLfda6KxydT2N45ZRvbuvvxwd87dXPuPKpdsuVS8fGyJtF9uRh", "9gpvHG2Di5xtJ4xUaeCoiSVeUQQ7CDbSyr1foPYmXZzEnjc8nzM", "9hqYbfsgXCVTzGukoB9j2B9HfC4B9Spyt7qEVXv4fnSCz3KFGLJ", "9i53Ub4bvkUMsK3ypKq3G6YU2E4651XcBFvAGReCWA4QHEyBDKg")
    val oraclesPri = Seq("518de2eeb828f3c69930905c06ef8278c80dacf808833da6bfea092247e1b70a", "5a9641e6f0450f137e1fde2b86dbaee07f217ba0b02e4ada5bd0033d6aa6b058", "800282b1276d6343dc335fbf4ba92df9c60fb2b63fda565e3ea6898b49c892e8", "705f8879042a32ceec1c41b89895d17393c88f42ee8c38278ddd8c1cc7138c5d", "b2862b04e9d0ebd2ab142f7f834080d6e2c07dd9be6a65e5dea51e3405174e30")
    val oraclesPrivateKey = oraclesPri.map(BigInt(_, 16))


    def createOracle(txB: UnsignedTransactionBuilder, tokenBox: InputBox, oracleContract: ErgoContract, oracles: Seq[String], oraclesPrivateKey: Seq[BigInt]): OutBox = {
      val bftValue = ErgoValue.of(3)
      val oraclesAddress = oracles.map(Address.create(_).getPublicKeyGE.getEncoded)
      val msg = oraclesAddress.fold(JavaHelpers.collFrom("".getBytes))(_.append(_))
      val signs = oraclesPrivateKey.map(sign(msg.toArray, _))
      val oraclesValue = ErgoValue.of(IndexedSeq(oraclesAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))

      val signs_a = ErgoValue.of(signs.map(sign => sign._1).toArray, ErgoType.groupElementType)
      val signs_z = ErgoValue.of(signs.map(sign => sign._2).toArray, ErgoType.bigIntType)
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(bftValue, oraclesValue, signs_a, signs_z)
        .contract(oracleContract)
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(createOracle(txB, tokenBox, oracleContract, oracles, oraclesPrivateKey), CreateChangeBoxes(txB, boxFee, feeAmount, our))
      .fee(feeAmount)
      .withDataInputs(Seq(gravityBox).asJava)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("OracleBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter("OracleBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def createPulseBox(ctx: BlockchainContext, prover: ErgoProver, boxes: List[InputBox], oracleBox: InputBox, pulseContract: ErgoContract, pulseTokenId: String): Unit = {

    val tokenBox = boxes.filter(box => {
      box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(pulseTokenId)
    }).head
    val boxFee = boxes.filter(box => box.getTokens.size() == 0).head
    val txB = ctx.newTxBuilder()

    val feeAmount = 1000000L

    val oracles = Seq("9gM6oXmjA63KKKiLkXhKB2wTD7zugaMJhjaXx6cKvt8kJjiu5xx", "9gLfda6KxydT2N45ZRvbuvvxwd87dXPuPKpdsuVS8fGyJtF9uRh", "9gpvHG2Di5xtJ4xUaeCoiSVeUQQ7CDbSyr1foPYmXZzEnjc8nzM", "9hqYbfsgXCVTzGukoB9j2B9HfC4B9Spyt7qEVXv4fnSCz3KFGLJ", "9i53Ub4bvkUMsK3ypKq3G6YU2E4651XcBFvAGReCWA4QHEyBDKg")
    val oraclesPri = Seq("518de2eeb828f3c69930905c06ef8278c80dacf808833da6bfea092247e1b70a", "5a9641e6f0450f137e1fde2b86dbaee07f217ba0b02e4ada5bd0033d6aa6b058", "800282b1276d6343dc335fbf4ba92df9c60fb2b63fda565e3ea6898b49c892e8", "705f8879042a32ceec1c41b89895d17393c88f42ee8c38278ddd8c1cc7138c5d", "b2862b04e9d0ebd2ab142f7f834080d6e2c07dd9be6a65e5dea51e3405174e30")
    val oraclesPrivateKey = oraclesPri.map(BigInt(_, 16))


    def createPulse(txB: UnsignedTransactionBuilder, tokenBox: InputBox, pulseContract: ErgoContract, oraclesPrivateKey: Seq[BigInt]): OutBox = {
      val msgData = Base16.encode("create first pulse box".getBytes).getBytes()
      val msgValue = ErgoValue.of(msgData)
      val signs = oraclesPrivateKey.map(sign(msgData, _))
      val pulseId = ErgoValue.of(2000L)

      val signs_a = ErgoValue.of(signs.map(sign => sign._1).toArray, ErgoType.groupElementType)
      val signs_z = ErgoValue.of(signs.map(sign => sign._2).toArray, ErgoType.bigIntType)
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(msgValue, signs_a, signs_z, pulseId)
        .contract(pulseContract)
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: InputBox, feeAmount: Long, ownerAddress: Address): OutBox = {
      txB.outBoxBuilder
        .value(inputFeeBox.getValue - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()
    }

    val tx = txB.boxesToSpend(Seq(tokenBox, boxFee).asJava)
      .outputs(createPulse(txB, tokenBox, pulseContract, oraclesPrivateKey), CreateChangeBoxes(txB, boxFee, feeAmount, our))
      .fee(feeAmount)
      .withDataInputs(Seq(oracleBox).asJava)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)

    new PrintWriter("PulseBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("PulseBox_tx.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def run(ctx: BlockchainContext): Unit = {
    val secret = BigInt("a73febe82f334157832ea12ed92e0a4969bb52534e2cc03daec6b94bc0c13cd6", 16)
    val prover = ctx.newProverBuilder()
      .withDLogSecret(secret.bigInteger)
      .build()

    val gravityScript: String =
      s"""{
         |  val newConsuls = OUTPUTS(0).R5[Coll[Coll[Byte]]].get
         |  // make Coll[GroupElement] for sign validation from input's consuls witch are in [Coll[Coll[Byte]]] format
         |  val consuls: Coll[GroupElement] = SELF.R5[Coll[Coll[Byte]]].get.map({(consul: Coll[Byte]) => decodePoint(consul)})
         |
         |  // each sign made two part a (a groupelemet) and z(a bigint)
         |  val signs_a = OUTPUTS(0).R6[Coll[GroupElement]].get
         |  val signs_z = OUTPUTS(0).R7[Coll[BigInt]].get
         |
         |  // get round and lastRound value
         |  val round = OUTPUTS(0).R8[Long].get
         |  val lastRound = SELF.R8[Long].get
         |  // making the message by concatenation of newConsoles
         |  val msg = newConsuls(0) ++ newConsuls(1) ++ newConsuls(2) ++ newConsuls(3) ++ newConsuls(4) ++ longToByteArray(round)
         |
         | // Verify sign base on schnorr protocol
         |  val validateSign = {(v: ((Coll[Byte], GroupElement), (GroupElement, BigInt))) => {
         |     val e: Coll[Byte] = blake2b256(v._1._1) // weak Fiat-Shamir
         |     val eInt = byteArrayToBigInt(e) // challenge as big integer
         |     val g: GroupElement = groupGenerator
         |     val l = g.exp(v._2._2)
         |     val r = v._2._1.multiply(v._1._2.exp(eInt))
         |     if (l == r) 1 else 0
         |  }}
         |
         |  // validate each sign and consul
         |  val count = validateSign( ( (msg, consuls(0)), (signs_a(0), signs_z(0)) ) ) +
         |              validateSign( ( (msg, consuls(1)), (signs_a(1), signs_z(1)) ) ) +
         |              validateSign( ( (msg, consuls(2)), (signs_a(2), signs_z(2)) ) ) +
         |              validateSign( ( (msg, consuls(3)), (signs_a(3), signs_z(3)) ) ) +
         |              validateSign( ( (msg, consuls(4)), (signs_a(4), signs_z(4)) ) )
         |
         |  val bftValueIn = SELF.R4[Int].get
         |  val bftValueOut = OUTPUTS(0).R4[Int].get
         |  sigmaProp (
         |    allOf(Coll(
         |      round > lastRound,
         |
         |      // check output's bftvalue be valid
         |      bftValueIn == bftValueOut,
         |      OUTPUTS(0).propositionBytes == SELF.propositionBytes,
         |      OUTPUTS(0).value >= SELF.value,     // value of output should be bigger or equal to input's
         |      OUTPUTS(0).tokens(0)._1 == tokenId, // Build-time assignment, it's the NFT tocken
         |      OUTPUTS(0).tokens(0)._2 == 1,       // check NFT count
         |
         |       // check count be bigger than input's bftvalue. to change the consuls,
         |       // it's important to sign at least equal to input's bftvalue
         |      count >= bftValueIn
         |
         |  )))
         |}""".stripMargin

    val signalScript: String =
      s"""{
         | sigmaProp(allOf(Coll(
         |  // To prevent placing two signal boxes in one transaction
         |  SELF.id == INPUTS(0).id,
         |
         |  // Expect pulseId to be in R4 of the signal box
         |  SELF.R4[Long].isDefined,
         |  // There must be data in the R5 of the signal box
         |  // TODO: this data must be equal to msgHash in pulseId
         |  SELF.R5[Coll[Byte]].isDefined,
         |
         |  // Id of first token in signal box must be equal to tokenRepoId with value 1
         |  SELF.tokens(0)._1 == tokenRepoId,
         |  SELF.tokens(0)._2 == 1,
         |
         |  // Contract of second INPUT must be equal to tokenRepoContractHash
         |  blake2b256(INPUTS(1).propositionBytes) == tokenRepoContractHash,
         |  // Id of first token in token repo box must be equal to tokenRepoId
         |  INPUTS(1).tokens(0)._1 == tokenRepoId,
         |
         |  // Contract of first OUTPUT must be equal to tokenRepoContractHash
         |  blake2b256(OUTPUTS(0).propositionBytes) == tokenRepoContractHash
         | )))
         |}""".stripMargin

    val tokenRepoScript: String =
      s"""{
         | val checkPulse = {allOf(Coll(
         |  // Contract of new tokenRepo box must be equal to contract of tokenRepo box in input
         |  SELF.propositionBytes == OUTPUTS(1).propositionBytes,
         |  // Id of first token in tokenRepo box must be equal to tokenRepoId
         |  SELF.tokens(0)._1 == tokenRepoId,
         |  // The transaction in which the tokenRepo box is located as the input box must contain the first input box containing the pulseNebulaNFT token
         |  INPUTS(0).tokens(0)._1 == pulseNebulaNFT,
         |  // OUTPUTS(1) is box of tokenRepo, OUTPUTS(2) is box of signal
         |  // In scenario add_pulse, a token is transferred from the tokenRepo to the signal box, also the minValue value must be sent to the signal box.
         |  OUTPUTS(1).tokens(0)._1 == tokenRepoId,
         |  OUTPUTS(1).tokens(0)._2 == SELF.tokens(0)._2 - 1,
         |  OUTPUTS(1).value == SELF.value - minValue,
         |  OUTPUTS(2).tokens(0)._1 == tokenRepoId,
         |  OUTPUTS(2).tokens(0)._2 == 1,
         |  OUTPUTS(2).value == minValue
         | ))}
         | // In scenario spend signal box in USER-SC, the token in the signal  box and its Erg must be returned to the tokenRepo.
         | val checkSignal = {allOf(Coll(
         |  OUTPUTS(0).value == SELF.value + minValue,
         |  OUTPUTS(0).tokens(0)._1 == tokenRepoId,
         |  OUTPUTS(0).tokens(0)._2 == INPUTS(1).tokens(0)._2 + 1,
         |  OUTPUTS(0).propositionBytes == SELF.propositionBytes
         | ))}
         | sigmaProp(checkPulse || checkSignal)
         |}""".stripMargin

    val pulseScript: String =
      s"""{
         | // We expect msgHash to be in R4
         | val msgHash = OUTPUTS(0).R4[Coll[Byte]].get
         |
         | // We expect first option of signs to be in R6 [a, a, ..] TODO: after fix AOT in ergo this can be change to [(a, z), (a, z), ...]
         | val signs_a = OUTPUTS(0).R5[Coll[GroupElement]].get
         | // We expect second option of signs to be in R7 [z, z, ..]
         | val signs_z = OUTPUTS(0).R6[Coll[BigInt]].get
         |
         | val currentPulseId = SELF.R7[Long].get
         | val signalCreated: Int = SELF.R8[Int].get
         |
         | // Verify signs
         | val validateSign: Int = {(v: ((Coll[Byte], GroupElement), (GroupElement, BigInt))) => {
         |    val e: Coll[Byte] = blake2b256(v._1._1) // weak Fiat-Shamir
         |    val eInt = byteArrayToBigInt(e) // challenge as big integer
         |    val g: GroupElement = groupGenerator
         |    val l = g.exp(v._2._2)
         |    val r = v._2._1.multiply(v._1._2.exp(eInt))
         |    if (l == r) 1 else 0
         | }}
         |
         | val publicCheckOutBoxes: Boolean = {(box: (Box, Box)) => {
         |    allOf(Coll(
         |      // We expect one tokenNFT for pulse contract to be in token(0)
         |      box._2.tokens(0)._1 == pulseNebulaNFT,
         |      // Value of new pulse box must be greater than equal to value of pulse box in input
         |      box._2.value >= box._1.value,
         |      // Contract of new pulse box must be equal to contract of pulse box in input
         |      box._2.propositionBytes == box._1.propositionBytes
         |    ))
         | }}
         |
         | val verified = if (signalCreated == 1) {
         |    // should to be box of oracle contract
         |    val dataInput = CONTEXT.dataInputs(0)
         |    // We Expect number of oracles that verified msgHash of in pulseId bigger than bftValue
         |    val check_bftCoefficient = {
         |      // We expect one tokenNFT for oracle contract to be in token(0) of this box
         |      if (dataInput.tokens(0)._1 == oracleNebulaNFT) {
         |        // get BftCoefficient from R4 of oracleContract Box
         |        val bftValue = dataInput.R4[Int].get
         |        // Get oracles from R5 of oracleContract Box and convert to Coll[GroupElement]
         |        val oracles: Coll[GroupElement] = dataInput.R5[Coll[Coll[Byte]]].get.map({ (oracle: Coll[Byte]) =>
         |            decodePoint(oracle)
         |        })
         |        val count : Int= validateSign(((msgHash, oracles(0)),(signs_a(0), signs_z(0)))) + validateSign(((msgHash, oracles(1)),(signs_a(1), signs_z(1)))) + validateSign(((msgHash, oracles(2)),(signs_a(2), signs_z(2)))) + validateSign(((msgHash, oracles(3)),(signs_a(3), signs_z(3)))) + validateSign(((msgHash, oracles(4)),(signs_a(4), signs_z(4))))
         |        count >= bftValue
         |      }
         |     else false
         |    }
         |    val checkOUTPUTS = {
         |     if(SELF.tokens(0)._1 == pulseNebulaNFT) {
         |     val dataType = OUTPUTS(0).R9[Int].get
         |      allOf(Coll(
         |        publicCheckOutBoxes((SELF, OUTPUTS(0))),
         |        // We expect pulseId to be in R7 and increase pulseId in out box
         |        OUTPUTS(0).R7[Long].get == currentPulseId + 1,
         |        OUTPUTS(0).R8[Int].get == 0,
         |        dataType == SELF.R9[Int].get,
         |        dataType >= 0,
         |        dataType < 3
         |
         |      ))
         |     }
         |     else false
         |    }
         |    (check_bftCoefficient && checkOUTPUTS)
         |  } else {
         |     val checkRegisters = {
         |      val dataType = OUTPUTS(0).R9[Int].get
         |
         |      allOf(Coll(
         |         SELF.R4[Coll[Byte]].get == msgHash,
         |         SELF.R5[Coll[GroupElement]].get == signs_a,
         |         SELF.R6[Coll[BigInt]].get == signs_z,
         |         // We expect pulseId to be in R7 and increase pulseId in out box
         |         OUTPUTS(0).R7[Long].get == currentPulseId,
         |         OUTPUTS(0).R8[Int].get == 1,
         |         dataType == SELF.R9[Int].get,
         |         dataType >= 0,
         |         dataType < 3,
         |
         |         // Expect pulseId to be in R4 of the signal box
         |         OUTPUTS(2).R4[Long].get == currentPulseId,
         |         // There must be data in the R5 of the signal box
         |         // TODO: this data must be equal to msgHash
         |         OUTPUTS(2).R5[Coll[Byte]].isDefined
         |      ))
         |     }
         |     val checkOUTPUTS = {
         |       if(SELF.tokens(0)._1 == pulseNebulaNFT) {
         |        allOf(Coll(
         |          publicCheckOutBoxes((SELF, OUTPUTS(0))),
         |
         |          // Contract of second INPUT/OUTPUT must be equal to tokenRepoContractHash
         |          blake2b256(INPUTS(1).propositionBytes) == tokenRepoContractHash,
         |          blake2b256(OUTPUTS(1).propositionBytes) == tokenRepoContractHash,
         |
         |          // Contract of third OUTPUT must be equal to signalContractHash
         |          blake2b256(OUTPUTS(2).propositionBytes) == signalContractHash
         |        ))
         |       }
         |       else false
         |    }
         |    (checkRegisters && checkOUTPUTS)
         |  }
         |
         |
         | sigmaProp ( verified )
         |
         | }
    """.stripMargin

    val oracleScript: String =
      s"""{
         | // We get oracles from R5
         | val newSortedOracles = OUTPUTS(0).R5[Coll[Coll[Byte]]].get
         |
         | // We expect first option of signs to be in R6 [a, a, ..] TODO: after fix AOT in ergo this can be change to [(a, z), (a, z), ...]
         | val signs_a = OUTPUTS(0).R6[Coll[GroupElement]].get
         | // We expect first option of signs to be in R7 [z, z, ..]
         | val signs_z = OUTPUTS(0).R7[Coll[BigInt]].get
         |
         | // should to be box of gravity contract
         | val dataInput = CONTEXT.dataInputs(0)
         |
         | // Verify signs
         | val validateSign = {(v: ((Coll[Byte], GroupElement), (GroupElement, BigInt))) => {
         |    val e: Coll[Byte] = blake2b256(v._1._1) // weak Fiat-Shamir
         |    val eInt = byteArrayToBigInt(e) // challenge as big integer
         |    val g: GroupElement = groupGenerator
         |    val l = g.exp(v._2._2)
         |    val r = v._2._1.multiply(v._1._2.exp(eInt))
         |    if (l == r) 1 else 0
         | }}
         |
         | val check_bftCoefficient = {
         |   // We expect in tokens of gravity contract there is NFT token of gravity also five oracles at R5 of OUTPUTS(0)
         |   if (dataInput.tokens(0)._1 == gravityNFT && newSortedOracles.size == 5) {
         |     // We get bftCoefficient from R4
         |     val bftValueIn = SELF.R4[Int].get
         |     val bftValueOut = OUTPUTS(0).R4[Int].get
         |     // We expect in R5 of gravity contract there are consuls
         |     val consuls: Coll[GroupElement] = dataInput.R5[Coll[Coll[Byte]]].get.map({ (consul: Coll[Byte]) =>
         |       decodePoint(consul)
         |     })
         |     // Concatenation all new oracles for create, newSortedOracles as a Coll[Byte] and verify signs.
         |     val newSortedOracles1 = newSortedOracles(0) ++ newSortedOracles(1) ++ newSortedOracles(2) ++ newSortedOracles(3) ++ newSortedOracles(4)
         |     val count = validateSign(((newSortedOracles1, consuls(0)),(signs_a(0), signs_z(0)))) + validateSign(((newSortedOracles1, consuls(1)),(signs_a(1), signs_z(1)))) + validateSign(((newSortedOracles1, consuls(2)),(signs_a(2), signs_z(2)))) + validateSign(((newSortedOracles1, consuls(3)),(signs_a(3), signs_z(3)))) + validateSign(((newSortedOracles1, consuls(4)),(signs_a(4), signs_z(4))))
         |     // We Expect the numbers of consuls that verified the new oracles list, to be more than three. TODO: in the future, with a change in the contract, this parameter can be dynamic.
         |     bftValueIn == bftValueOut && count >= bftValueIn
         |   }
         |  else false
         | }
         |
         | val checkOUTPUT = {
         |   if(SELF.tokens(0)._1 == oracleNebulaNFT) {
         |    allOf(Coll(
         |      // We expect a NFT token for oracle contract to be in tokens(0)
         |      OUTPUTS(0).tokens(0)._1 == oracleNebulaNFT,
         |
         |      // Value of new oracle box must be greater than equal to value of oracle box in input
         |      OUTPUTS(0).value >= SELF.value,
         |      // Contract of new oracle box must be equal to contract of oracle box in input
         |      OUTPUTS(0).propositionBytes == SELF.propositionBytes
         |    ))
         |   }
         |   else false
         | }
         |
         | sigmaProp ( checkOUTPUT && check_bftCoefficient )
         |
         | }
    """.stripMargin


    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList

    println(boxes)

    println("\n\t\t\tissuing gravityTokenId:")
    val gravityTokenId: String = issueNFTToken(prover, boxes, "Gravity_NFT_V0.1", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing oracleTokenId:")
    val oracleTokenId: String = issueNFTToken(prover, boxes, "Oracle_NFT_V0.1", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing pulseTokenId:")
    val pulseTokenId: String = issueNFTToken(prover, boxes, "Pulse_NFT_V0.1", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing tokenRepoTokenId:")
    val tokenRepoTokenId: String = issueToken(prover, boxes, "TokenRepo_V0.1", "Gravity Project: https://gravity.tech/")

    val tokenRepoContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("tokenRepoId", ErgoId.create(tokenRepoTokenId).getBytes)
        .item("pulseNebulaNFT", ErgoId.create(pulseTokenId).getBytes)
        .item("minValue", 1000000L)
        .build(),
      tokenRepoScript
    )
    val tokenRepoErgoTree: ErgoTree = tokenRepoContract.getErgoTree
    val tokenRepoHash: Digest32 = scorex.crypto.hash.Blake2b256(tokenRepoErgoTree.bytes)

    lazy val signalContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("tokenRepoContractHash", tokenRepoHash)
        .item("tokenRepoId", ErgoId.create(tokenRepoTokenId).getBytes)
        .build(),
      signalScript
    )
    val signalErgoTree: ErgoTree = signalContract.getErgoTree
    val signalHash: Digest32 = scorex.crypto.hash.Blake2b256(signalErgoTree.bytes)

    lazy val pulseContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("oracleNebulaNFT", ErgoId.create(oracleTokenId).getBytes)
        .item("pulseNebulaNFT", ErgoId.create(pulseTokenId).getBytes)
        .item("tokenRepoContractHash", tokenRepoHash)
        .item("signalContractHash", signalHash)
        .build(),
      pulseScript
    )

    lazy val oracleContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("gravityNFT", ErgoId.create(gravityTokenId).getBytes)
        .item("oracleNebulaNFT", ErgoId.create(oracleTokenId).getBytes)
        .build(),
      oracleScript
    )

    val gravityContract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("tokenId", ErgoId.create(gravityTokenId).getBytes)
        .build(),
      gravityScript
    )


    println("\n\t\t\tcreateTokenRepoBox:")
    createTokenRepoBox(ctx, prover, boxes, tokenRepoContract, tokenRepoTokenId)
    println("\n\t\t\tcreateGravityBox:")
    createGravityBox(ctx, prover, boxes, gravityContract, gravityTokenId)
    println("\n\t\t\tchangeGravityBox:")
    changeGravityBox(ctx, prover, boxes, gravityContract, gravityTokenId)
    println("\n\t\t\tcreateOracleBox:")
    createOracleBox(ctx, prover, boxes, ctx.getUnspentBoxesFor(Address.create(addrEnc.fromProposition(gravityContract.getErgoTree).get.toString)).asScala.toList.head, oracleContract, oracleTokenId)
    println("\n\t\t\tcreatePulseBox:")
    createPulseBox(ctx, prover, boxes, ctx.getUnspentBoxesFor(Address.create(addrEnc.fromProposition(oracleContract.getErgoTree).get.toString)).asScala.toList.head, pulseContract, pulseTokenId)
  }
}
