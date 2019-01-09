package com.gu.support.workers

import com.gu.i18n.Currency
import com.gu.support.encoding.Codec
import com.gu.support.encoding.Codec.deriveCodec
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import cats.syntax.functor._

sealed trait DeliveryZone { def asString: String }

case object Domestic extends DeliveryZone {
  override def asString: String = "domestic"
}

case object RestOfTheWorld extends DeliveryZone {
  override def asString: String = "rest_of_the_world"
}

object DeliveryZone {

  val allZones = List(Domestic, RestOfTheWorld)

  def fromString(string: String): Option[DeliveryZone] = allZones.find(_.asString == string)

  implicit val encodeDeliveryZone: Encoder[DeliveryZone] = Encoder.encodeString.contramap[DeliveryZone](_.asString)

  implicit val decodeDeliveryZone: Decoder[DeliveryZone] = Decoder.decodeString.emap {
    identifier => DeliveryZone.fromString(identifier).toRight(s"Unrecognised delivery zone '$identifier'")
  }

}

sealed trait FulfilmentMethod { def asString: String }

case object HomeDelivery extends FulfilmentMethod {
  override def asString: String = "home_delivery"
}

case object Voucher extends FulfilmentMethod {
  override def asString: String = "voucher"
}

object FulfilmentMethod {

  val allFulfilmentMethods = List(HomeDelivery, Voucher)

  def fromString(string: String): Option[FulfilmentMethod] = allFulfilmentMethods.find(_.asString == string)

  implicit val encodeFulfilmentMethod: Encoder[FulfilmentMethod] = Encoder.encodeString.contramap[FulfilmentMethod](_.asString)

  implicit val decodeFulfilmentMethod: Decoder[FulfilmentMethod] = Decoder.decodeString.emap {
    identifier => FulfilmentMethod.fromString(identifier).toRight(s"Unrecognised package '$identifier'")
  }

}

sealed trait NewspaperPackage { def asString: String }

case object Everyday extends NewspaperPackage {
  override def asString: String = "everyday"
}

case object Sixday extends NewspaperPackage {
  override def asString: String = "sixday"
}

case object Weekend extends NewspaperPackage {
  override def asString: String = "weekend"
}

case object Sunday extends NewspaperPackage {
  override def asString: String = "sunday"
}

case object `Everyday+` extends NewspaperPackage {
  override def asString: String = "everyday+"
}

case object `Sixday+` extends NewspaperPackage {
  override def asString: String = "sixday+"
}

case object `Weekend+` extends NewspaperPackage {
  override def asString: String = "weekend+"
}

case object `Sunday+` extends NewspaperPackage {
  override def asString: String = "sunday+"
}

object NewspaperPackage {

  val printOnly = List(Everyday, Sixday, Weekend, Sunday)
  val printAndDigital = List(`Everyday+`, `Sixday+`, `Weekend+`, `Sunday+`)
  val allPackages: List[NewspaperPackage] = printOnly ++ printAndDigital

  def fromString(string: String): Option[NewspaperPackage] = allPackages.find(_.asString == string)

  implicit val encodeNewspaperPackage: Encoder[NewspaperPackage] = Encoder.encodeString.contramap[NewspaperPackage](_.asString)

  implicit val decodeNewspaperPackage: Decoder[NewspaperPackage] = Decoder.decodeString.emap {
    identifier => NewspaperPackage.fromString(identifier).toRight(s"Unrecognised package '$identifier'")
  }

}

sealed trait ProductType {
  override def toString: String = this.getClass.getSimpleName
  def describe: String
}

case class Contribution(
  amount: BigDecimal,
  currency: Currency,
  billingPeriod: BillingPeriod
) extends ProductType {
  override def describe: String = s"$billingPeriod-Contribution-$currency-$amount"
}

case class DigitalPack(
  currency: Currency,
  billingPeriod: BillingPeriod
) extends ProductType {
  override def describe: String = s"$billingPeriod-DigitalPack-$currency"
}

case class GuardianWeekly(
  currency: Currency,
  billingPeriod: BillingPeriod,
  deliveryZone: DeliveryZone
) extends ProductType {
  override def describe: String = s"$billingPeriod-$deliveryZone-GuardianWeekly-$currency"
}

case class Newspaper(
 `package`: NewspaperPackage,
 fulfilmentMethod: FulfilmentMethod
) extends ProductType {
  override def describe: String = s"Newspaper-$fulfilmentMethod-${`package`}"
}

object ProductType {
  import com.gu.support.encoding.CustomCodecs._
  implicit val codecDigitalPack: Codec[DigitalPack] = deriveCodec
  implicit val codecGuardianWeekly: Codec[GuardianWeekly] = deriveCodec
  implicit val codecNewspaper: Codec[Newspaper] = deriveCodec
  implicit val codecContribution: Codec[Contribution] = deriveCodec

  implicit val encodeProduct: Encoder[ProductType] = Encoder.instance {
    case d: DigitalPack => d.asJson
    case w: GuardianWeekly => w.asJson
    case n: Newspaper => n.asJson
    case c: Contribution => c.asJson
  }

  implicit val decodeProduct: Decoder[ProductType] =
    List[Decoder[ProductType]](
      Decoder[Contribution].widen,
      Decoder[GuardianWeekly].widen,
      Decoder[Newspaper].widen,
      Decoder[DigitalPack].widen,
    ).reduceLeft(_ or _)
}
