package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to encode functional domain
 * constructors and operators:
 *
 * 1. Using a function or interface, whose methods execute the solution. This is
 *    called the "executable" encoding in this course. It's a direct, executable
 *    encoding of a domain. If some functional domain is modeled with a class
 *    or case class, or an open trait that is implemented by classes, then it's
 *    probably an executable encoding.
 *
 * 2. Using a pure data structure, which declaratively describes the solution, but
 *    which does not perform the solution. It's an abstract, "declarative"
 *    encoding of a domain. If some functional domain type is modeled with a
 *    sealed trait, then it's probably an abstract encoding, where the subtypes
 *    of the sealed trait model individual operations and constructors in the
 *    domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which merely models a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the functional domain.
 *
 * Executable encodings are "open": anyone can add new constructors and
 * operators, without updating existing code. On the other hand, executable
 * encodings are not "introspectable": because they are not data, but rather,
 * opaque executable machinery, they cannot be serialized, optimized, or
 * converted to other encodings.
 *
 * Abstract encodings are "introspectable": because they are pure data, they
 * can be serialized, optimized, and converted to other encodings, assuming
 * their component parts have the same properties (not all abstract encodings
 * do; if you embed a function inside an abstract encoding, it's becomes
 * opaque). On the other hand, abstract encodings are "closed": no one can add
 * new constructors or operators, without updating existing code.
 *
 * Legacy code prefers executable encodings; while many benefits of Functional
 * Design can be seen best using abstract encodings.
 *
 */

/**
 * EDUCATION - EXERCISE SET 1
 *
 * Consider a console-based educational application that tests the user's
 * knowledge of key concepts.
 */
object education_executable {
  import education._

  sealed trait Quiz2 { self =>

    /**
     * EXERCISE 1
     *
     * Add an operator `+` that appends this quiz to the specified quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def +(that: Quiz2): Quiz2 = Quiz2.Append(self, that)

    /**
     * EXERCISE 2
     *
     * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def bonus: Quiz2 = Quiz2.Bonus(self)
  }
  object Quiz2 {
    final case class Append(left: Quiz2, right: Quiz2)      extends Quiz2
    final case class Bonus(value: Quiz2)                    extends Quiz2
    final case class FromQuestion[A](question: Question[A]) extends Quiz2

    def apply[A](question: Question[A]): Quiz2 = FromQuestion(question)
  }

  /**
   * EXERCISE 3
   *
   * Implement an interpreter for the `Quiz` model that translates it into
   * the interactive console operations that it describes, returning a
   * QuizResult value.
   */
  def run(quiz: Quiz2): QuizResult = {
    def translate(quiz: Quiz2): Quiz = quiz match {
      case Quiz2.FromQuestion(question) => Quiz(question)
      case Quiz2.Bonus(value)           => translate(value).bonus
      case Quiz2.Append(l, r)           => translate(l) + translate(r)
    }
    translate(quiz).run()
  }
}

/**
 * DATA TRANSFORM - EXERCISE SET 2
 *
 * Consider an email marketing platform, which allows users to upload contacts.
 */
object contact_processing2 {
  import contact_processing._

  sealed trait SchemaMapping2 { self =>

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that models combining two schema mappings into one,
     * applying the effects of both in sequential order.
     */
    def +(that: SchemaMapping2): SchemaMapping2 = SchemaMapping2.AndThen(self, that)

    /**
     * EXERCISE 2
     *
     * Add an `orElse` operator that models combining two schema mappings into
     * one, applying the effects of the first one, unless it fails, and in that
     * case, applying the effects of the second one.
     */
    def orElse(that: SchemaMapping2): SchemaMapping2 = SchemaMapping2.OrElse(self, that)
  }
  object SchemaMapping2 {

    case class AndThen(a: SchemaMapping2, b: SchemaMapping2) extends SchemaMapping2
    case class OrElse(a: SchemaMapping2, b: SchemaMapping2)  extends SchemaMapping2
    case class Rename(oldName: String, newName: String)      extends SchemaMapping2
    case class Delete(name: String)                          extends SchemaMapping2

    /**
     * EXERCISE 3
     *
     * Add a constructor for `SchemaMapping` models renaming the column name.
     */
    def rename(oldName: String, newName: String): SchemaMapping2 = Rename(oldName, newName)

    /**
     * EXERCISE 4
     *
     * Add a constructor for `SchemaMapping` that models deleting the column
     * of the specified name.
     */
    def delete(name: String): SchemaMapping2 = Delete(name)
  }

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `SchemaMapping` model that translates it into
   * into changes on the contact list.
   */
  import MappingResult._
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] =
    mapping match {
      case SchemaMapping2.AndThen(a, b) =>
        run(a, contacts) match {
          case Success(warnings, value) => run(b, value)
          case failure                  => failure
        }
      case SchemaMapping2.OrElse(a, b) =>
        run(a, contacts) match {
          case failure => run(b, contacts)
          case success => success
        }
      case SchemaMapping2.Delete(name) =>
        Success(Nil, contacts.delete(name))
      case SchemaMapping2.Rename(oldName, newName) =>
        Success(Nil, contacts.rename(oldName, newName))

    }

  /**
   * EXERCISE 6
   *
   * Implement an optimizer for the `SchemaMapping` model that detects and eliminates
   * redundant renames; e.g. renaming "name" to "first_name", and then back to "name".
   */
  def optimize(schemaMapping: SchemaMapping2): SchemaMapping2 =
    ???
}

/**
 * EMAIL CLIENT - EXERCISE SET 3
 *
 * Consider a web email interface, which allows users to filter emails and
 * direct them to specific folders based on custom criteria.
 */
object email_filter2 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  sealed trait EmailFilter { self =>

    /**
     * EXERCISE 1
     *
     * Add an "and" operator that models matching an email if both the first and
     * the second email filter match the email.
     */
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    /**
     * EXERCISE 2
     *
     * Add an "or" operator that models matching an email if either the first or
     * the second email filter match the email.
     */
    def ||(that: EmailFilter): EmailFilter = EmailFilter.Or(self, that)

    /**
     * EXERCISE 3
     *
     * Add a "negate" operator that models matching an email if this email filter
     * does NOT match an email.
     */
    def negate: EmailFilter = EmailFilter.Negate(self)
  }
  object EmailFilter {

    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class Or(left: EmailFilter, right: EmailFilter)  extends EmailFilter
    final case class Negate(value: EmailFilter)                 extends EmailFilter
    final case class SubjectContains(value: String)             extends EmailFilter
    final case class BodyContains(value: String)                extends EmailFilter
    final case class SenderIn(senders: Set[Address])            extends EmailFilter
    final case class RecipientIn(recipients: Set[Address])      extends EmailFilter

    /**
     * EXERCISE 4
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * subject of an email contains the specified word.
     */
    def subjectContains(string: String): EmailFilter = SubjectContains(string)

    /**
     * EXERCISE 5
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * body of an email contains the specified word.
     */
    def bodyContains(string: String): EmailFilter = BodyContains(string)

    /**
     * EXERCISE 6
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * sender of an email is in the specified set of senders.
     */
    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    /**
     * EXERCISE 7
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * recipient of an email is in the specified set of recipients.
     */
    def recipientIn(recipients: Set[Address]): EmailFilter = recipientIn(recipients)
  }

  /**
   * EXERCISE 8
   *
   * Implement an interpreter for the `EmailFilter` model that translates it into
   * into tests on the specified email.
   */
  import EmailFilter._
  def matches(filter: EmailFilter, email: Email): Boolean =
    filter match {
      case And(left, right) => matches(left, email) && matches(right, email)
      case Or(left, right)  => matches(left, email) || matches(right, email)
      case Negate(filter)   => !matches(filter, email)

      case SubjectContains(phrase) => email.subject.contains(phrase)
      case BodyContains(phrase)    => email.body.contains(phrase)
      case SenderIn(set)           => set.contains(email.sender)
      case RecipientIn(set)        => email.to.exists(set.contains)
    }

  /**
   * EXERCISE 9
   *
   * Implement a function to print out an English-readable description of an
   * `EmailFilter`.
   */
  def describe(filter: EmailFilter): String = filter match {
    case EmailFilter.And(left, right)        => "(" + describe(left) + " && " + describe(right) + ")"
    case EmailFilter.Or(left, right)         => "(" + describe(left) + " || " + describe(right) + ")"
    case EmailFilter.Negate(value)           => "!" + describe(value)
    case EmailFilter.SubjectContains(string) => "subject contains: " + string
    case EmailFilter.BodyContains(string)    => "body contains: " + string
    case EmailFilter.SenderIn(senders)       => "sender in: " + senders.mkString("(", ",", ")")
    case EmailFilter.RecipientIn(recipients) => "recipient in: " + recipients.mkString("(", ",", ")")
  }
}

/**
 * SPREADSHEET - EXERCISE SET 4
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet2 {
  trait Spreadsheet {
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CellContents

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CellContents)

  sealed trait CellContents
  object CellContents {
    sealed trait Static extends CellContents
    object Static {
      final case class Error(message: String) extends Static
      final case class Str(value: String)     extends Static
      final case class Dbl(value: Double)     extends Static
    }

    sealed trait Expr extends CellContents {

      /**
       * EXERCISE 1
       *
       * Add some operators to transform one `Expr` into another `Expr`. For
       * example, one operator could "negate" a double expression.
       */
      def negate: Expr = ???

      /**
       * EXERCISE 2
       *
       * Add some operators to combine `Expr`. For example, one operator
       * could sum two double expressions.
       */
      def sum(that: Expr): Expr = ???
    }
    object Expr {

      /**
       * EXERCISE 3
       *
       * Add a constructor that makes an Expr from a CellContents.
       */
      def const(contents: CellContents): Expr = ???

      /**
       * EXERCISE 4
       *
       * Add a constructor that provides access to the value of the
       * specified cell, identified by col/row.
       */
      def at(col: Int, row: Int): Expr = ???
    }
  }

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `CellContents.Expr` model that translates it into
   * static cell contents by evaluating the expression.
   */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): CellContents.Static = ???
}

/**
 * E-COMMERCE MARKETING - GRADUATION PROJECT
 *
 * Consider an e-commerce marketing platform where emails are sent to users
 * whose history matches specific patterns (for example, an event of adding
 * a product to a shopping card, followed by an abandonment of the web
 * session).
 */
object ecommerce_marketing {
  type Event = Map[Attribute, Value]

  sealed trait Attribute
  object Attribute {
    case object EventType      extends Attribute
    case object UserName       extends Attribute
    case object ShoppingCartId extends Attribute
    case object Email          extends Attribute
    case object WebSession     extends Attribute
    case object DateTime       extends Attribute
  }

  sealed trait Value
  object Value {
    final case class Str(value: String)                        extends Value
    final case class Id(value: String)                         extends Value
    final case class Email(value: String)                      extends Value
    final case class DateTime(value: java.time.OffsetDateTime) extends Value
  }

  object abstract_encoding {
    sealed trait Pattern { self =>
      def +(that: Pattern): Pattern = Pattern.Sequence(self, that)

      def repeat(min: Option[Int], max: Option[Int]): Pattern = Pattern.Repeat(self, min, max)
    }
    object Pattern {

      final case class HasAttribute(attr: Attribute)                                extends Pattern
      object HasAnyAttribute                                                        extends Pattern
      final case class HasValue(attr: Attribute, value: Value)                      extends Pattern
      final case class Sequence(first: Pattern, second: Pattern)                    extends Pattern
      final case class Repeat(pattern: Pattern, min: Option[Int], max: Option[Int]) extends Pattern
    }
    import Pattern._

    def matches(history: List[Event], pattern: Pattern): Boolean = {
      def loop(history: List[Event], pattern: Pattern): (List[Event], Boolean) =
        (pattern, history.headOption) match {
          case (HasAttribute(attr), Some(event))    => (history.tail, event.contains(attr))
          case (HasAnyAttribute, Some(event))       => (history.tail, true)
          case (HasValue(attr, value), Some(event)) => (history.tail, event.get(attr).contains(value))
          case (Sequence(first, second), _) =>
            val (leftHistory, leftMatch) = loop(history, first)

            if (leftMatch) loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _) =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)) {
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)
            }

            if (!baseline._2) baseline
            else {
              val after = (0 to (max - min)).foldLeft(baseline) {
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)
              }

              (after._1, true)
            }
          case _ => (history, false)
        }
      loop(history, pattern)._2
    }
  }

  /**
   * EXERCISE 1
   *
   * Develop an executable encoding of the pattern matcher. Instead of having
   * an ADT to represent a pattern, and then interpreting that on a user
   * history to see if there is a match, you will represent a pattern as a
   * function or an interface that is capable of testing the user history for
   * a match.
   */
  object executable_encoding {
    final case class Pattern(loop: List[Event] => (List[Event], Boolean)) { self =>
      def matches(history: List[Event]): Boolean = loop(history)._2

      def +(that: Pattern): Pattern =
        Pattern { history =>
          val (history2, matches) = self.loop(history)

          if (!matches) (history2, matches)
          else that.loop(history2)
        }

      def repeat(min0: Option[Int], max0: Option[Int]): Pattern = Pattern { history =>
        val min = min0.getOrElse(0)
        val max = max0.getOrElse(Int.MaxValue)

        val baseline = (0 to min).foldLeft((history, true)) {
          case ((history, false), _) => (history, false)
          case ((history, true), _)  => self.loop(history)
        }

        if (!baseline._2) baseline
        else {
          val after = (0 to (max - min)).foldLeft(baseline) {
            case ((history, false), _) => (history, false)
            case ((history, true), _)  => self.loop(history)
          }

          (after._1, true)
        }
      }
    }
    object Pattern {

      def hasAttribute(attr: Attribute): Pattern =
        Pattern {
          case Nil                                 => (Nil, false)
          case head :: tail if head.contains(attr) => (tail, true)
          case _ :: tail                           => (tail, false)
        }
      def hasValue(attr: Attribute, value: Value): Pattern =
        Pattern {
          case Nil                                                        => (Nil, false)
          case head :: tail if head.contains(attr) && head(attr) == value => (tail, true)
          case _ :: tail                                                  => (tail, false)
        }
      val hasAnyAttribute: Pattern =
        Pattern {
          case Nil       => (Nil, false)
          case _ :: tail => (tail, true)
        }
    }
  }
}
