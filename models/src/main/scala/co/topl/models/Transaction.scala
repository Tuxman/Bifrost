package co.topl.models

import cats.data.Chain

case class Transaction(
  inputs:    Chain[Transaction.Input],
  outputs:   Chain[Transaction.Output],
  timestamp: Timestamp,
  data:      Option[TransactionData]
)

object Transaction {

  /**
   * @param transactionOutputIndex TODO: How does the network behave if we allow a huge number of outputs in a transaction?
   */
  case class Input(
    transactionId:          TypedIdentifier,
    transactionOutputIndex: Short,
    proposition:            Proposition,
    proof:                  Proof,
    value:                  Box.Value
  )
  case class Output(address: FullAddress, value: Box.Value, minting: Boolean)

  case class Unproven(
    inputs:    Chain[Transaction.Unproven.Input],
    outputs:   Chain[Transaction.Output],
    timestamp: Timestamp,
    data:      Option[TransactionData]
  )

  object Unproven {

    case class Input(
      transactionId:          TypedIdentifier,
      transactionOutputIndex: Short,
      proposition:            Proposition,
      value:                  Box.Value
    )
  }

}
