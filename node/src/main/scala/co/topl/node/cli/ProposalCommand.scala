package co.topl.node.cli

import cats.data.{EitherT, OptionT}
import cats.effect.{Async, Sync}
import cats.effect.std.Console
import cats.implicits._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.{Datum, Event, LockAddress}
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.numerics.implicits._

import scala.util.Try
import co.topl.node.models.Ratio
import com.google.protobuf.duration.Duration
import fs2.io.file.Path
import co.topl.node.cli.ProposalCommand.Messages
import co.topl.node.cli.ProposalCommand.Implicits._
import co.topl.node.cli.ProposalCommand.util._

object ProposalCommand {

  def apply[F[_]: Async: Console]: StageResultT[F, Unit] = new ProposalCommandImpl[F].command

  /**
   * If this approach is approved, this util should be moved to a common place, and use it on CliApp
   */
  object util {

    def read[F[_]: Sync, T](param: String, example: String)(implicit
      c: Console[F],
      f: String => Try[T]
    ): StageResultT[F, T] =
      (writeMessage[F](s"Please enter $param parameter. $example") >>
        readInput[F].semiflatMap(input =>
          EitherT
            .fromEither[F](f(input).toEither)
            .leftSemiflatTap(error => c.println(s"Invalid $param. Reason=$error input=$input"))
            .toOption
            .value
        )).untilDefinedM

    def readOptional[F[_]: Sync, T](param: String, example: String)(implicit
      c: Console[F],
      f: String => Try[T]
    ): StageResultT[F, Option[T]] =
      (writeMessage[F](s"Please enter $param parameter. $example") >>
        readInput[F].semiflatMap(input =>
          OptionT
            .fromOption[F](Option.when(input.isEmpty)(Option.empty[T]))
            .orElse(
              EitherT
                .fromEither[F](f(input).toEither.map(_.some))
                .leftSemiflatTap(error => c.println(s"Invalid $param. Reason=$error input=$input"))
                .toOption
            )
            .value
        )).untilDefinedM

  }

  object Implicits {
    implicit private[cli] val parseInt: String => Try[Int] = (s: String) => Try(s.toInt)
    implicit private[cli] val parseLong: String => Try[Long] = (s: String) => Try(s.toLong)

    implicit private[cli] val parseString: String => Try[String] = (s: String) =>
      Either.cond(s.nonEmpty, s, new IllegalArgumentException("Empty Input")).toTry

    implicit private[cli] val parseRatio: String => Try[Ratio] =
      (s: String) =>
        Try {
          s.split("/") match {
            case Array(numerator)              => Ratio(numerator.toInt, 1)
            case Array(numerator, denominator) => Ratio(numerator.toInt, denominator.toInt)
            case _                             => throw new IllegalArgumentException("Ratio parse error")
          }
        }

    implicit private[cli] val parseDuration: String => Try[Duration] = (s: String) =>
      Try(com.google.protobuf.duration.Duration(scala.concurrent.duration.Duration(s).toSeconds, 0))

    implicit private[cli] val parseLockAddress: String => Try[LockAddress] = (s: String) =>
      co.topl.brambl.codecs.AddressCodecs.decodeAddress(s).toTry

  }

  object Messages {

    val intro =
      "This tool will guide you through the process of preparing a Proposal Update. Press <Enter> to skip."

    val lockAddress = "Using a wallet (i.e. Brambl CLI), create or provide a new LockAddress."

    val writeTransaction = "This process will save the Proposal update in the current /tmp path"

    val finalMsg =
      "Your Update proposal has been saved. The Transaction that was saved should be imported into Brambl-CLI for input selection and broadcast."
  }
}

/**
 * Proposal Update command used by node cli app
 */
private class ProposalCommandImpl[F[_]: Async](implicit c: Console[F]) {

  val command: StageResultT[F, Unit] =
    for {
      _ <- writeMessage[F](Messages.intro)

      label                      <- read[F, String]("label <required>", "Ex: Update Slot duration")
      fEffective                 <- readOptional[F, Ratio]("f-effective", "Ex:18/2, 9/1, 9")
      vrfLddCutoff               <- readOptional[F, Int]("vrf-ldd-cutoff", "Ex:50")
      vrfPrecision               <- readOptional[F, Int]("vrf-precision", "Ex:40")
      vrfBaselineDifficulty      <- readOptional[F, Ratio]("vrf-baseline-difficulty", "Ex:1/20")
      vrfAmplitude               <- readOptional[F, Ratio]("vrf-amplitude", "Ex:1/2")
      chainSelectionKLookback    <- readOptional[F, Long]("chain-selection-k-lookback", "Ex:50")
      slotDuration               <- readOptional[F, Duration]("slot-duration", "Ex: 1000 milli")
      forwardBiasedSlotWindow    <- readOptional[F, Long]("forward-biased-slot-window", "Ex:50")
      operationalPeriodsPerEpoch <- readOptional[F, Long]("operational-periods-per-epoch", "Ex:2")
      kesKeyHours                <- readOptional[F, Int]("kes-key-hours", "Ex:2")
      kesKeyMinutes              <- readOptional[F, Int]("kes-key-minutes", "Ex:9")

      proposal = UpdateProposal(
        label,
        fEffective,
        vrfLddCutoff,
        vrfPrecision,
        vrfBaselineDifficulty,
        vrfAmplitude,
        chainSelectionKLookback,
        slotDuration,
        forwardBiasedSlotWindow,
        operationalPeriodsPerEpoch,
        kesKeyHours,
        kesKeyMinutes
      )

      lockAddress <- read[F, LockAddress]("Address", "Ex: ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr")

      _ <- writeMessage[F](Messages.lockAddress)
      //      _ <- requiresBramblCli // TODO refactor this method and print this message for BramblCLI

      unspentTransactionOutput =
        UnspentTransactionOutput(lockAddress, Value.defaultInstance.withUpdateProposal(proposal))

      transaction = IoTransaction(datum =
        Datum.IoTransaction(
          Event.IoTransaction.defaultInstance.withSchedule(
            Schedule(0L, Long.MaxValue, System.currentTimeMillis())
          )
        )
      ).withOutputs(Seq(unspentTransactionOutput))

      // TODO The name should be the id of the proposal Updated, path should be a user input
      _ <- writeFile[F](Path("/tmp"))(transaction.toByteArray)(
        "Proposal Update",
        "proposalUpdate.transaction.pbuf"
      )
      _ <- writeMessage[F](Messages.writeTransaction)
      _ <- writeMessage[F](Messages.finalMsg)
    } yield StageResult.Menu

}
