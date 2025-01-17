package net.degoes

import net.degoes.ecommerce_marketing.abstract_encoding.EventPattern
import net.degoes.education_executable.Quiz2.Bonus

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
 * Executable encodings are "open" for new constructors and operators: anyone
 * can add new constructors and operators, without updating existing code. On
 * the other hand, executable encodings are not "introspectable": because they
 * are not data, but rather, opaque executable machinery, it is not possible to
 * add new ways to execute the models without rewriting all constructors and
 * operators.
 *
 * Declarative encodings are "closed" for new constructors and operators: no
 * one can add new constructors and operators, without updating existing code.
 * Yet, because they are pure data, it is easy to add new ways to execute the
 * models, for example, serializers, optimizers, converters, and so forth,
 * assuming their component parts have the same properties (not all
 * declarative encodings do; if you embed a function inside a declarative
 * encoding, it becomes opaque).
 *
 * Summarizing the difference between executable and abstract encodings:
 *
 *  - Executable encodings have unbounded constructors/operators, but a fixed
 *    number of ways to execute them.
 *  - Declarative encodings have fixed constructors/operators, but an unbounded
 *    number of ways to execute them.
 *
 * Note: Tagless-final is an executable encoding, but one where, by making the
 * "solutions" polymorphic, the choice of executor can be deferred arbitrarily.
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
    def +(that: Quiz2): Quiz2 = Quiz2.Sequence(this, that)

    /**
     * EXERCISE 2
     *
     * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def bonus: Quiz2 = Bonus(this)

    def run(): QuizResult = QuizResult.empty
  }
  object Quiz2 {
    final case class Sequence(left: Quiz2, right: Quiz2) extends Quiz2 // constructor
    final case class Bonus(quiz: Quiz2)                  extends Quiz2 // operator
    final case class Single[A](question: Question[A])    extends Quiz2 // operator

    def apply[A](question: Question[A]): Quiz2 = Single(question)
  }

  val executed = (4 + 2) * 2

  sealed trait IntExpr
  object IntExpr {
    final case class Lit(v: Int)                        extends IntExpr
    final case class Add(left: IntExpr, right: IntExpr) extends IntExpr
    final case class Mul(left: IntExpr, right: IntExpr) extends IntExpr

    val described = Mul(Add(Lit(4), Lit(2)), Lit(2))
  }

  import Quiz2._

  /**
   * EXERCISE 3
   *
   * Implement an interpreter for the `Quiz` model that translates it into
   * the interactive console operations that it describes, returning a
   * QuizResult value.
   */
  def run(quiz: Quiz2): QuizResult = quiz match {
    case Single(q)             => Quiz2(q).run()
    case Sequence(left, right) => run(left) + run(right)
    case Bonus(q)              => q.run().toBonus
  }

}

/**
 * DATA TRANSFORM - EXERCISE SET 2
 *
 * Consider an email marketing platform, which allows users to upload contacts.
 */
object contact_processing2 {
  import SchemaMapping2._
  import contact_processing._

  sealed trait SchemaMapping2 {

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that models combining two schema mappings into one,
     * applying the effects of both in sequential order.
     */
    def +(that: SchemaMapping2): SchemaMapping2 = Sequence(this, that)

    /**
     * EXERCISE 2
     *
     * Add an `orElse` operator that models combining two schema mappings into
     * one, applying the effects of the first one, unless it fails, and in that
     * case, applying the effects of the second one.
     */
    def orElse(that: SchemaMapping2): SchemaMapping2 = Fallback(this, that)
  }
  object SchemaMapping2 {
    final case class Rename(oldName: String, newName: String)                extends SchemaMapping2
    final case class Delete(name: String)                                    extends SchemaMapping2
    final case class Sequence(left: SchemaMapping2, right: SchemaMapping2)   extends SchemaMapping2
    final case class Fallback(first: SchemaMapping2, second: SchemaMapping2) extends SchemaMapping2

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
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] = mapping match {
    case Rename(oldName, newName) => SchemaMapping.rename(oldName, newName).map(contacts)
    case Delete(name)             => SchemaMapping.delete(name).map(contacts)
    case Sequence(left, right)    => run(left, contacts).flatMap(x => run(right, x))
    case Fallback(first, second)  => run(first, contacts).orElse(run(second, contacts))
  }

  /**
   * BONUS EXERCISE
   *
   * Implement an optimizer for the `SchemaMapping` model that pushes deletes to the front of the
   * schema mapping in cases where doing so wouldn't later the result.
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

  import EmailFilter._

  sealed trait EmailFilter { self =>

    /**
     * EXERCISE 1
     *
     * Add an "and" operator that models matching an email if both the first and
     * the second email filter match the email.
     */
    def &&(that: EmailFilter): EmailFilter = And(this, that)

    /**
     * EXERCISE 2
     *
     * Add an "or" operator that models matching an email if either the first or
     * the second email filter match the email.
     */
    def ||(that: EmailFilter): EmailFilter = Or(this, that)

    /**
     * EXERCISE 3
     *
     * Add a "negate" operator that models matching an email if this email filter
     * does NOT match an email.
     */
    def negate: EmailFilter = Not(this)
  }
  object EmailFilter {
    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class Or(left: EmailFilter, right: EmailFilter)  extends EmailFilter
    final case class Not(v: EmailFilter)                        extends EmailFilter
    final case class SubjectContains(v: String)                 extends EmailFilter
    final case class BodyContains(v: String)                    extends EmailFilter
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
    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)
  }

  /**
   * EXERCISE 8
   *
   * Implement an interpreter for the `EmailFilter` model that translates it into
   * into tests on the specified email.
   */
  def matches(filter: EmailFilter, email: Email): Boolean = filter match {
    case And(left, right)        => matches(left, email) && matches(right, email)
    case Or(left, right)         => matches(left, email) || matches(right, email)
    case Not(v)                  => matches(v, email)
    case SubjectContains(v)      => email.subject.contains(v)
    case BodyContains(v)         => email.body.contains(v)
    case SenderIn(senders)       => senders.contains(email.sender)
    case RecipientIn(recipients) => recipients.contains(email.sender)
  }

  /**
   * EXERCISE 9
   *
   * Implement a function to make an English-readable description of an
   * `EmailFilter`.
   */
  def describe(filter: EmailFilter): Unit = ??? // trivial, skipping
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

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): LazyList[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).to(LazyList)
        row <- (minRow to maxRow).to(LazyList)
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  sealed trait Value
  object Value {
    final case class Error(message: String) extends Value
    final case class Str(value: String)     extends Value
    final case class Dbl(value: Double)     extends Value
  }

  import CalculatedValue._

  sealed trait CalculatedValue { self =>

    /**
     * EXERCISE 1
     *
     * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
     * example, one operator could "negate" a double CalculatedValue.
     */
    def negate: CalculatedValue = Not(this)

    /**
     * EXERCISE 2
     *
     * Add some operators to combine `CalculatedValue`. For example, one operator
     * could sum two double CalculatedValueessions.
     */
    def sum(that: CalculatedValue): CalculatedValue = Add(this, that)
  }
  object CalculatedValue {
    final case class Not(v: CalculatedValue)                            extends CalculatedValue
    final case class Add(left: CalculatedValue, right: CalculatedValue) extends CalculatedValue
    final case class Const(v: Value)                                    extends CalculatedValue
    final case class At(col: Int, row: Int)                             extends CalculatedValue

    /**
     * EXERCISE 3
     *
     * Add a constructor that makes an CalculatedValue from a Value.
     */
    def const(contents: Value): CalculatedValue = Const(contents)

    /**
     * EXERCISE 4
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue = At(col, row)
  }

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `Value.CalculatedValue` model that translates it into
   * static cell contents by evaluating the CalculatedValueession.
   */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): Value = spreadsheet.valueAt(cell.col, cell.row) match {
    case Not(v) =>
      evaluate(spreadsheet, cell.copy(contents = v)) match {
        case Value.Error(_) | Value.Str(_) => Value.Str("error")
        case Value.Dbl(value)              => Value.Dbl(value + 1) // negate
      }
    case Add(left, right) => ??? // like above
    case Const(v)         => v
    case At(col, row)     => evaluate(spreadsheet, cell.copy(col = col, row = row))
  }
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
    case object EventType extends Attribute {
      val AddItem    = "add_item"
      val RemoveItem = "remove_item"
      val Abandon    = "abandon"
    }
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
    sealed trait HistoryPattern { self =>
      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern.Sequence(self, that)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern = HistoryPattern.Repeat(self, min, max)
    }
    object HistoryPattern {
      case object Matches                                                                  extends HistoryPattern
      final case class EventP(eventPattern: EventPattern)                                  extends HistoryPattern
      final case class Sequence(first: HistoryPattern, second: HistoryPattern)             extends HistoryPattern
      final case class Repeat(pattern: HistoryPattern, min: Option[Int], max: Option[Int]) extends HistoryPattern

      val matches: HistoryPattern = Matches

      def event(eventPattern: EventPattern): HistoryPattern = EventP(eventPattern)

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
    }
    sealed trait EventPattern { self =>
      import EventPattern._

      def matches(event: Event): Boolean =
        self match {
          case Matches               => true
          case HasValue(attr, value) => event.get(attr) == Some(value)
        }
    }
    object EventPattern {
      case object Matches                                      extends EventPattern
      final case class HasValue(attr: Attribute, value: Value) extends EventPattern

    }
    import Attribute.EventType
    import HistoryPattern._

    val example = eventType(EventType.AddItem) *> eventType(EventType.Abandon)

    def matches(history: List[Event], pattern: HistoryPattern): Boolean = {
      def loop(history: List[Event], pattern: HistoryPattern): (List[Event], Boolean) =
        (pattern, history.headOption) match {
          case (EventP(eventPattern), Some(event)) => (history.tail, eventPattern.matches(event))
          case (EventP(_), None)                   => (history.tail, false)
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

    final case class HistoryPattern(matches: (List[Event]) => Boolean) {
      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern { history =>
        this.matches(List(history.head)) && that.matches(history.tail)
      }

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern = ???
    }

    object HistoryPattern {
      val matches: HistoryPattern = HistoryPattern(_ => true)

      def event(eventPattern: EventPattern): HistoryPattern = HistoryPattern {
        case head :: _ => eventPattern.matches(head)
        case Nil       => false
      }

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
    }
  }
}
