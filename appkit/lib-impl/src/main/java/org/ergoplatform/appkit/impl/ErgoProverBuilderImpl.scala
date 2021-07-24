package org.ergoplatform.appkit.impl

import org.ergoplatform.appkit._
import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import sigmastate.basics.{DLogProtocol, DiffieHellmanTupleProverInput}
import special.sigma.GroupElement
import java.math.BigInteger
import java.util

class ErgoProverBuilderImpl(_ctx: BlockchainContextImpl) extends ErgoProverBuilder {
  private var _masterKey: ExtendedSecretKey = _
  private val _dhtSecrets = new util.ArrayList[DiffieHellmanTupleProverInput]
  private val _dLogSecrets = new util.ArrayList[DLogProtocol.DLogProverInput]

  override def withMnemonic(mnemonicPhrase: SecretString,
                            mnemonicPass: SecretString): ErgoProverBuilder = {
    _masterKey = JavaHelpers.seedToMasterKey(mnemonicPhrase, mnemonicPass)
    this
  }

  override def withMnemonic(mnemonic: Mnemonic): ErgoProverBuilder =
    withMnemonic(mnemonic.getPhrase, mnemonic.getPassword)

  override def withSecretStorage(storage: SecretStorage): ErgoProverBuilder = {
    if (storage.isLocked)
      throw new IllegalStateException("SecretStorage is locked, call unlock(password) method")
    _masterKey = storage.getSecret
    this
  }

  override def withDHTData(g: GroupElement,
                           h: GroupElement,
                           u: GroupElement,
                           v: GroupElement,
                           x: BigInteger): ErgoProverBuilder = {
    val dht = JavaHelpers.createDiffieHellmanTupleProverInput(g, h, u, v, x)
    if (_dhtSecrets.contains(dht))
      throw new IllegalStateException("DHTuple secret already exists")
    _dhtSecrets.add(dht)
    this
  }

  override def withDLogSecret(x: BigInteger): ErgoProverBuilder = {
    val dLog = new DLogProtocol.DLogProverInput(x)
    if (_dLogSecrets.contains(dLog))
      throw new IllegalStateException("Dlog secret already exists")
    _dLogSecrets.add(dLog)
    this
  }

  override def build: ErgoProver = {
    val parameters = new ErgoLikeParameters() {
      private[impl] val _params = _ctx.getNodeInfo.getParameters

      override def storageFeeFactor: Int = _params.getStorageFeeFactor
      override def minValuePerByte: Int = _params.getMinValuePerByte
      override def maxBlockSize: Int = _params.getMaxBlockSize
      override def tokenAccessCost: Int = _params.getTokenAccessCost
      override def inputCost: Int = _params.getInputCost
      override def dataInputCost: Int = _params.getDataInputCost
      override def outputCost: Int = _params.getOutputCost
      override def maxBlockCost: Long = _params.getMaxBlockCost.longValue()
      override def softForkStartingHeight: Option[Int] = ???
      override def softForkVotesCollected: Option[Int] = ???
      override def blockVersion: Byte = _params.getBlockVersion.byteValue
    }
    val keys = new util.ArrayList[ExtendedSecretKey]
    if (_masterKey != null)
      keys.add(_masterKey)
    val interpreter = new AppkitProvingInterpreter(keys, _dLogSecrets, _dhtSecrets, parameters)
    new ErgoProverImpl(_ctx, interpreter)
  }
}

