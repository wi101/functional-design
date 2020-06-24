package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.ww
 *
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = !self && !that

    def ^^(that: EmailFilter): EmailFilter = (self || that) && (self && that).negate

    def negate: EmailFilter = EmailFilter.Not(self)

    def unary_! : EmailFilter = negate
  }
  object EmailFilter {

    sealed trait Criteria
    object Criteria {
      case object Body    extends Criteria
      case object Subject extends Criteria
    }
    final case object Always                                    extends EmailFilter
    final case class Not(v: EmailFilter)                        extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
//    final case class InclusiveOr(left: EmailFilter, right: EmailFilter) extends EmailFilter
//    final case class ExclusiveOr(left: EmailFilter, right: EmailFilter) extends EmailFilter
//    final case class SenderEquals(target: Address)                      extends EmailFilter
//    final case class SenderNotEquals(target: Address)                   extends EmailFilter
//    final case class RecipientEquals(target: Address)                   extends EmailFilter
//    final case class RecipientNotEquals(target: Address)                extends EmailFilter
    final case class SenderIn(targets: Set[Address])    extends EmailFilter
    final case class RecipientIn(targets: Set[Address]) extends EmailFilter
//    final case class BodyContains(phrase: String)                       extends EmailFilter
//    final case class BodyNotContains(phrase: String)                    extends EmailFilter
//    final case class SubjectContains(phrase: String)                    extends EmailFilter
//    final case class SubjectNotContains(phrase: String)                 extends EmailFilter
    final case class Contains(criteria: Criteria, phrase: String) extends EmailFilter
    val always: EmailFilter = !senderIn(Set())

    val never: EmailFilter = !always

    def senderIs(sender: Address): EmailFilter = SenderIn(Set(sender))

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIs(recipient: Address): EmailFilter = recipientIn(Set(recipient))

    def recipientIsNot(recipient: Address): EmailFilter = !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = Contains(Criteria.Body, phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = Contains(Criteria.Subject, phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait Turtle { self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  //Declarative
  sealed trait Turtle2 { self =>
    def +(that: Turtle2): Turtle2 = Turtle2.AndThen(self, that)

    def run(turtle: Turtle2)(turtleImpl: Turtle): Unit = ???
//      tortoise match {
//        case Turn(angle) =>
//          turtle.turnLeft(angle.toInt)
//
//        case MoveForward =>
//          turtle.goForward()
//
//        case Draw =>
//          turtle.draw()
//
//        case Then(left, right) =>
//          eval(left)(turtle)
//          eval(right)(turtle)
//      }
  }

  object Turtle2 {
    type NonNegativeDouble = Double
    case class AndThen(a: Turtle2, b: Turtle2)    extends Turtle2
    case object Draw                              extends Turtle2
    case class TurnLeft(angle: NonNegativeDouble) extends Turtle2
    case object Go                                extends Turtle2

    val moveForward: Turtle2           = Go
    val moveBackward: Turtle2          = turn(180.0) + moveForward + turn(180.0)
    def turn(degrees: Double): Turtle2 = if (degrees < 0) TurnLeft(degrees) else TurnLeft(360 - degrees)
  }

  // Executable
  final case class Turtle3(run: Turtle => Unit) { self =>
    def +(that: Turtle3): Turtle3 = Turtle3 { turtle =>
      self.run(turtle)
      that.run(turtle)
    }
  }
  object Turtle3 {
    def turnLeft(degrees: Int): Turtle3 = Turtle3(_.turnLeft(degrees))

    def turnRight(degrees: Int): Turtle3 = Turtle3(_.turnLeft(degrees))

    val goForward: Turtle3 = Turtle3(_.goForward())

    val goBackward: Turtle3 = Turtle3(_.goBackward())

    val draw: Turtle3 = Turtle3(_.draw())
  }
}
