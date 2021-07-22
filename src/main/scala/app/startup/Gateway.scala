package app.startup

import app.helpers.Configs
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
import scala.util.control.Breaks._

import java.io.{PrintWriter, File}
import java.math.BigInteger
import scala.collection.JavaConverters._

object Gateway {
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
    Configs.addressEncoder.fromProposition(contract.getErgoTree).get.toString
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

      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      new PrintWriter(s"result_gateway/${tokenName}_signed.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_gateway/${tokenName}_txId.txt") {
        write(txId)
        close()
      }
      println(txId)
      (id, signed.getOutputsToSpend.get(0).getId.toString)
    })
    //    tokenId
  }

  def issueToken(prover: ErgoProver, box: InputBox, tokenName: String, tokenDescription: String): (String, String) = {
    Utils.ergoClient.execute((ctx: BlockchainContext) => {

      val txB = ctx.newTxBuilder()
      val id = box.getId.toString
      val issuingNum = 1000000L
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

      val signed: SignedTransaction = prover.sign(tx)
      println(signed.toJson(false))
      new PrintWriter(s"result_gateway/${tokenName}_signed.txt") {
        write(signed.toJson(false))
        close()
      }
      val txId = ctx.sendTransaction(signed)
      new PrintWriter(s"result_gateway/${tokenName}_txId.txt") {
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

  def createTokenRepoBox(ctx: BlockchainContext, prover: ErgoProver, feeBoxes: List[InputBox], tokenRepoContract: ErgoContract, tokenRepoTokenBoxId: String): Unit = {
    val tokenBox = ctx.getBoxesById(tokenRepoTokenBoxId).head
    var boxes = feeBoxes.filter(box => box.getValue > 100 * Configs.defaultTxFee)
    var ergBox: List[InputBox] = List()
    var total = 0L
    object AllDone extends Exception {}

    while (total < 1000000000000L) {
      total = 0L
      for (box <- boxes) {
        total += box.getValue
        ergBox = box :: ergBox
        if (total == 1000000000000L) {
          throw AllDone
        }
      }
      if (total < 1000000000000L) {
        println("Not enough erg, waiting for more ergs ...")
        Thread.sleep(2 * 60 * 1000)
        boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getTokens.size() == 0)
      }
    }


    val txB = ctx.newTxBuilder()

    val addressTokenRepo = Address.create(Configs.addressEncoder.fromProposition(tokenRepoContract.getErgoTree).get.toString)

    def CreateTokenBox(txB: UnsignedTransactionBuilder, numToken: Long, tokenBox: InputBox, addressTokenRepo: Address) = {
      txB.outBoxBuilder
        .value(1000000L * numToken)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, numToken))
        .contract(new ErgoTreeContract(addressTokenRepo.getErgoAddress.script))
        .build()
    }

    def CreateChangeBoxes(txB: UnsignedTransactionBuilder, inputFeeBox: List[InputBox], total: Long, tokenBox: InputBox, numToken: Long, numTokenBox: Int, feeAmount: Long, ownerAddress: Address): Seq[OutBox] = {
      val changeTokenBox = txB.outBoxBuilder
        .value(feeAmount)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, tokenBox.getTokens.get(0).getValue - (numToken * numTokenBox)))
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      val changeFeeBox = txB.outBoxBuilder
        .value(total - (1000000L * numToken * numTokenBox) - feeAmount)
        .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .build()

      Seq(changeTokenBox, changeFeeBox)
    }

    val numTokenBox = 10
    var outboxes: Seq[OutBox] = List.range(0, numTokenBox).map(x => CreateTokenBox(txB, 100, tokenBox, addressTokenRepo))
    outboxes = outboxes ++ CreateChangeBoxes(txB, ergBox, total, tokenBox, 100L, numTokenBox, Configs.defaultTxFee, our)
    val tx = txB.boxesToSpend((Seq(tokenBox) ++ ergBox).asJava)
      .outputs(outboxes: _*)
      .fee(Configs.defaultTxFee)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("result_gateway/TokenRepoBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("result_gateway/TokenRepoBox_txId.txt") {
      write(txId)
      close()
    }
    println(txId)
  }

  def createGravityBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, gravityContract: ErgoContract, gravityTokenBoxId: String): Unit = {

    val tokenBox = ctx.getBoxesById(gravityTokenBoxId).head

    val txB = ctx.newTxBuilder()

    val consuls = Configs.consulsPk
    val consulsPri = Configs.consulsPriv
    val consulsPrivateKey = consulsPri.map(BigInt(_, 16))


    def createGravity(txB: UnsignedTransactionBuilder, tokenBox: InputBox, gravityContract: ErgoContract, consuls: Seq[String], consulsPrivateKey: Seq[BigInt]): OutBox = {
      val bftValue = ErgoValue.of(3)
      val consulsAddress = consuls.map(Address.create(_).getPublicKeyGE.getEncoded)
      val msg = consulsAddress.fold(JavaHelpers.collFrom("".getBytes))(_.append(_))
      val signs = consulsPrivateKey.map(sign(msg.toArray, _))
      val consulsValue = ErgoValue.of(IndexedSeq(consulsAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))
      val lastround = ErgoValue.of(0)

      val signs_a = ErgoValue.of(signs.map(sign => sign._1).toArray, ErgoType.groupElementType)
      val signs_z = ErgoValue.of(signs.map(sign => sign._2).toArray, ErgoType.bigIntType)
      txB.outBoxBuilder
        .value(tokenBox.getValue)
        .tokens(new ErgoToken(tokenBox.getTokens.get(0).getId, 1))
        .registers(bftValue, consulsValue, signs_a, signs_z, lastround)
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
      .outputs(createGravity(txB, tokenBox, gravityContract, consuls, consulsPrivateKey),
        CreateChangeBoxes(txB, boxFee, Configs.defaultTxFee, our))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("result_gateway/GravityBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    println(signed.toJson(false))
    val txId = ctx.sendTransaction(signed)
    new PrintWriter("result_gateway/GravityBox_txId.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def createOracleBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, gravityBox: InputBox, oracleContract: ErgoContract, oracleTokenBoxId: String): Unit = {
    val tokenBox = ctx.getBoxesById(oracleTokenBoxId).head
    val txB = ctx.newTxBuilder()

    val oracles = Configs.oraclesPk
    val oraclesPri = Configs.oraclesPriv
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
      .outputs(createOracle(txB, tokenBox, oracleContract, oracles, oraclesPrivateKey),
        CreateChangeBoxes(txB, boxFee, Configs.defaultTxFee, our))
      .fee(Configs.defaultTxFee)
      .withDataInputs(Seq(gravityBox).asJava)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("result_gateway/OracleBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter("result_gateway/OracleBox_txId.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def createPulseBox(ctx: BlockchainContext, prover: ErgoProver, boxFee: InputBox, oracleBox: InputBox, pulseContract: ErgoContract, pulseTokenBoxId: String): Unit = {
    val tokenBox = ctx.getBoxesById(pulseTokenBoxId).head

    val txB = ctx.newTxBuilder()

    val oracles = Configs.oraclesPk
    val oraclesPri = Configs.oraclesPriv
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
      .outputs(createPulse(txB, tokenBox, pulseContract, oraclesPrivateKey),
        CreateChangeBoxes(txB, boxFee, Configs.defaultTxFee, our))
      .fee(Configs.defaultTxFee)
      .withDataInputs(Seq(oracleBox).asJava)
      .sendChangeTo(our.getErgoAddress)
      .build()

    val signed: SignedTransaction = prover.sign(tx)
    new PrintWriter("result_gateway/PulseBox_signed.txt") {
      write(signed.toJson(false));
      close()
    }
    val txId = ctx.sendTransaction(signed)
    println(signed.toJson(false))
    new PrintWriter("result_gateway/PulseBox_txId.txt") {
      write(txId);
      close()
    }
    println(txId)

  }

  def run(ctx: BlockchainContext): Unit = {


    val secret = BigInt(Configs.proverSecret, 16)
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
         | // should to be box of oracle contract
         | val dataInput = CONTEXT.dataInputs(0)
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
         | // We Expect number of oracles that verified msgHash of in pulseId bigger than bftValue
         | val check_bftCoefficient = {
         |   // We expect one tokenNFT for oracle contract to be in token(0) of this box
         |   if (dataInput.tokens(0)._1 == oracleNebulaNFT) {
         |     // get BftCoefficient from R4 of oracleContract Box
         |     val bftValue = dataInput.R4[Int].get
         |     // Get oracles from R5 of oracleContract Box and convert to Coll[GroupElement]
         |     val oracles: Coll[GroupElement] = dataInput.R5[Coll[Coll[Byte]]].get.map({ (oracle: Coll[Byte]) =>
         |         decodePoint(oracle)
         |     })
         |     val count : Int= validateSign(((msgHash, oracles(0)),(signs_a(0), signs_z(0)))) + validateSign(((msgHash, oracles(1)),(signs_a(1), signs_z(1)))) + validateSign(((msgHash, oracles(2)),(signs_a(2), signs_z(2)))) + validateSign(((msgHash, oracles(3)),(signs_a(3), signs_z(3)))) + validateSign(((msgHash, oracles(4)),(signs_a(4), signs_z(4))))
         |     count >= bftValue
         |   }
         |  else false
         | }
         |
         | val checkOUTPUTS = {
         |   if(SELF.tokens(0)._1 == pulseNebulaNFT) {
         |    allOf(Coll(
         |      // We expect one tokenNFT for pulse contract to be in token(0)
         |      OUTPUTS(0).tokens(0)._1 == pulseNebulaNFT,
         |      // Value of new pulse box must be greater than equal to value of pulse box in input
         |      OUTPUTS(0).value >= SELF.value,
         |      // Contract of new pulse box must be equal to contract of pulse box in input
         |      OUTPUTS(0).propositionBytes == SELF.propositionBytes,
         |      // We expect pulseId to be in R7 and increase pulseId in out box
         |      OUTPUTS(0).R7[Long].get == SELF.R7[Long].get + 1,
         |
         |      // Contract of second INPUT/OUTPUT must be equal to tokenRepoContractHash
         |      blake2b256(INPUTS(1).propositionBytes) == tokenRepoContractHash,
         |      blake2b256(OUTPUTS(1).propositionBytes) == tokenRepoContractHash,
         |
         |      // Contract of third OUTPUT must be equal to signalContractHash
         |      blake2b256(OUTPUTS(2).propositionBytes) == signalContractHash,
         |      // There must be a msgHash in the R4 of the signal box
         |      OUTPUTS(2).R4[Coll[Byte]].get == msgHash
         |    ))
         |   }
         |   else false
         | }
         |
         | sigmaProp ( check_bftCoefficient && checkOUTPUTS )
         |
         | }
    """.stripMargin

    val signalScript: String =
      s"""{
         | sigmaProp(allOf(Coll(
         |  // There must be a msgHash in the R4 of the signal box
         |  SELF.R4[Coll[Byte]].isDefined,  // TODO: In the future, we have to check the msgHash for the USER-SC.
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

    val boxes = ctx.getUnspentBoxesFor(our).asScala.toList.filter(box => box.getValue > 2 * Configs.defaultTxFee)
    println(s"size: ${
      boxes.size
    }")

    println("\n\t\t\tissuing gravityTokenId:")
    val (gravityTokenId, gravityTokenBoxId: String) = issueNFTToken(prover, boxes.head, "Gravity_NFT_sigma", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing oracleTokenId:")
    val (oracleTokenId, oracleTokenBoxId: String) = issueNFTToken(prover, boxes.drop(1).head, "Oracle_NFT_sigma", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing pulseTokenId:")
    val (pulseTokenId, pulseTokenBoxId: String) = issueNFTToken(prover, boxes.drop(2).head, "Pulse_NFT_sigma", "Gravity Project: https://gravity.tech/")
    println("\n\t\t\tissuing tokenRepoTokenId:")
    val (tokenRepoTokenId, tokenRepoTokenBoxId: String) = issueToken(prover, boxes.drop(3).head, "TokenRepo_sigma", "Gravity Project: https://gravity.tech/")

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

    val feeBoxes = boxes.drop(4).filter(box => box.getTokens.size() == 0)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(gravityTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    println("\n\t\t\tcreateGravityBox:")
    createGravityBox(ctx, prover, feeBoxes.head, gravityContract, gravityTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(gravityContract.getErgoTree).get.toString)).asScala.toList.head
          ctx.getBoxesById(oracleTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    println("\n\t\t\tcreateOracleBox:")
    createOracleBox(ctx, prover, feeBoxes.drop(1).head, ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(gravityContract.getErgoTree).get.toString)).asScala.toList.head, oracleContract, oracleTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(oracleContract.getErgoTree).get.toString)).asScala.toList.head
          ctx.getBoxesById(pulseTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }

    println("\n\t\t\tcreatePulseBox:")
    createPulseBox(ctx, prover, feeBoxes.drop(2).head, ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(oracleContract.getErgoTree).get.toString)).asScala.toList.head, pulseContract, pulseTokenBoxId)

    breakable {
      while (true) {
        Thread.sleep(5 * 1000)
        try {
          ctx.getBoxesById(tokenRepoTokenBoxId)
          Thread.sleep(5 * 1000)
          break
        }
        catch {
          case e: Exception =>
        }
      }
    }
    println("\n\t\t\tcreateTokenRepoBox:")
    createTokenRepoBox(ctx, prover, feeBoxes.drop(3), tokenRepoContract, tokenRepoTokenBoxId)

  }
}
