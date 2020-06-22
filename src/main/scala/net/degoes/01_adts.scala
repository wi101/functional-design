package net.degoes

import java.time.Instant

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

object ADT {
  //ADT: any data type composed from sums and products, recursively
  //case class = product type
  final case class Person(name: String, age: Int)
  //sealed trait = sum types
  sealed trait Color
  case object Red  extends Color
  case object Blue extends Color
  // polymorphic types
  List[Int]
}

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  final case class CreditCard(number: BigInt, name: String, expirationDate: java.time.YearMonth, securityCode: Int)

  sealed abstract case class Name private (name: String)
  object Name {
    //Smart constructor
    def fromString(name: String): Option[Name] =
      if (name.trim.length == 0) None
      else Some(new Name(name) {})
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product
  sealed trait Physical extends Product
  sealed trait Digital  extends Product
  sealed trait Event    extends Product
  object Physical {
    case object Gallon extends Physical
    case object Milk   extends Physical
  }
  object Digital {
    case object Book   extends Digital
    case object Laptop extends Digital
  }
  object Event {
    case object Ticket extends Event
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme {
    def fee: BigDecimal
  }
  object PricingScheme {
    final case class OneTime(fee: BigDecimal)                                 extends PricingScheme
    final case class Recurring(fee: BigDecimal, duration: java.time.Duration) extends PricingScheme
  }
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
//
//  abstract class Event(val id: Int) {
//
//    def time: Instant
//  }
//
//  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
//  // please don't extend both it will break code!!!
//  trait UserEvent extends Event {
//    def userName: String
//  }
//
//  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
//  // please don't extend both it will break code!!!
//  trait DeviceEvent extends Event {
//    def deviceId: Int
//  }
//
//  class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
//      extends Event(id)
//      with DeviceEvent
//
//  class DeviceActivated(id: Int, val deviceId: Int, val time: Instant) extends Event(id) with DeviceEvent
//
//  class UserPurchase(id: Int, val item: String, val price: Double, val time: Instant, val userName: String)
//      extends Event(id)
//      with UserEvent
//
//  class UserAccountCreated(id: Int, val userName: String, val time: Instant) extends Event(id) with UserEvent

  /**
   * Solution 1
   */
  //  sealed trait Event
  //  sealed trait UserEvent   extends Event
  //  sealed trait DeviceEvent extends Event
  //
  //  object UserEvent {
  //    final case class SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double]) extends UserEvent
  //    final case class UserPurchase(id: Int, item: String, price: Double, time: Instant, userName: String)
  //        extends UserEvent
  //    final case class UserAccountCreated(id: Int, userName: String, time: Instant) extends UserEvent
  //  }
  //
  //  object DeviceEvent {
  //    final case class SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double]) extends DeviceEvent
  //    final case class DeviceActivated(id: Int, deviceId: Int, time: Instant)                        extends DeviceEvent
  //  }

  /**
   * Solution 2: I like it
   */
//  final case class Event(id: Int, time: Instant, event: EventType)
//
//  sealed trait EventType
//  sealed trait UserEvent   extends EventType
//  sealed trait DeviceEvent extends EventType
//  object UserEvent {
//    final case class UserPurchase(item: String, price: Double, userName: String) extends UserEvent
//    final case class UserAccountCreated(userName: String)                        extends UserEvent
//  }
//  object DeviceEvent {
//    final case class SensorUpdated(deviceId: Int, reading: Option[Double]) extends DeviceEvent
//    final case class DeviceActivated(deviceId: Int, time: Instant)         extends DeviceEvent
//  }

  /**
   * Solution 3: the same as 2 but more ADTs
   */
  final case class Event(id: Int, time: Instant, body: Payload)

  sealed trait Payload
  final case class UserEvent(userName: String) extends Payload

  sealed trait UserPayload
  object UserPayload {
    final case class Purchase(item: String, price: Double) extends UserPayload
    case object AccountCreated                             extends UserPayload
  }

  final case class DeviceEvent(deviceId: Int, devicePayload: DevicePayload) extends Payload

  sealed trait DevicePayload
  object DevicePayload {
    final case class SensorUpdated(reading: Option[Double]) extends DevicePayload
    case object DeviceActivated                             extends DevicePayload
  }
}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  final case class Document(ownerId: UserId, docId: DocId, content: DocContent)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  sealed trait AccessType { self =>
    import AccessType._
    def includeRead: Boolean =
      self match {
        case Read | ReadWrite => true
        case _                => false
      }
  }
  object AccessType {
    case object None      extends AccessType
    case object Read      extends AccessType
    case object Write     extends AccessType
    case object ReadWrite extends AccessType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  final case class DocPermissions(user: UserId, doc: DocId, accessType: Set[AccessType])

}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  type Customer

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  type AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  type Account
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
