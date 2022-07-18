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
 * smallest set of orthogonal primitives that exist.
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

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)

    def ^^(that: EmailFilter): EmailFilter = (self && !that) || (!self && that)

    def unary_! : EmailFilter = EmailFilter.Not(this)

  }
  object EmailFilter {
    final case object Always                                            extends EmailFilter
    final case object Never /* never is not !Always */                  extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter)         extends EmailFilter
    final case class Not(filter: EmailFilter)                           extends EmailFilter
    final case class InclusiveOr(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class SenderEquals(target: Address)                      extends EmailFilter
    final case class RecipientEquals(target: Address)                   extends EmailFilter
    final case class BodyContains(phrase: String)                       extends EmailFilter
    final case class SubjectContains(phrase: String)                    extends EmailFilter

    val acceptAll: EmailFilter = Always

    val rejectAll: EmailFilter = Never

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = !(SenderEquals(sender))

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = !(RecipientEquals(recipient))

    def senderIn(senders: Set[Address]): EmailFilter = senders.foldLeft(EmailFilter.rejectAll) {
      case (acc, sender) => acc || senderIs(sender)
    }

    def recipientIn(recipients: Set[Address]): EmailFilter = recipients.foldLeft(EmailFilter.rejectAll) {
      case (acc, recipient) => acc || recipientIs(recipient)
    }

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !(BodyContains(phrase))

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !SubjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components extends App {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  // assuming we cannot touch this interface - add a wrapper around it
  trait Turtle { self =>
    def turnLeft(degrees: Int) = println("left ")

    def turnRight(degrees: Int) = println("right ")

    def goForward() = println("fwd ")

    def goBackward() = println("back ")

    def draw() = println(".")
  }

  object executable {

    sealed trait MyDrawing {
      def execute(turtle: Turtle): Unit
    }

    object MyDrawing {
      case object EmptyMyDrawing extends MyDrawing {
        def execute(turtle: Turtle): Unit = ()
      }
      case object MyDrawingLive extends MyDrawing {
        def execute(turtle: Turtle): Unit = turtle.draw()
      }
    }

    final case class Drawing(execute: Turtle => Unit) {
      def *>(that: Drawing): Drawing = Drawing { turtle =>
        execute(turtle)
        that.execute(turtle)
      }

      def goForward: Drawing = this *> Drawing.goForward

      def turnLeft(degrees: Int): Drawing = this *> Drawing.turnLeft(degrees)

      def draw: Drawing = this *> Drawing.draw

      def repeat(n: Int): Drawing =
        if (n <= 0) Drawing.empty
        else this *> repeat(n - 1)
    }

    object Drawing {
      val empty: Drawing = Drawing(_ => ())

      val draw: Drawing = Drawing(turtle => turtle.draw())

      def goForward: Drawing = Drawing(t => t.goForward())

      def goBackward: Drawing = Drawing(t => t.goBackward())

      def turnLeft(degrees: Int): Drawing = Drawing { turtle =>
        turtle.turnLeft(degrees)
      }

      def turnRight(degrees: Int): Drawing = turnLeft(360 - degrees)
    }

    val mona: Drawing =
      Drawing.draw.goForward.turnLeft(90).repeat(4)

    mona.execute(new Turtle {})
  }

  object declarative {
    sealed trait Drawing {
      def *>(that: Drawing): Drawing = Drawing.Sequential(this, that)
      def draw: Drawing              = this *> Drawing.draw
      def goForward: Drawing         = this *> Drawing.goForward
      def goBackward: Drawing        = this *> Drawing.goBackward
      def repeat(n: Int): Drawing    = if (n <= 0) Drawing.blank else this *> this.repeat(n - 1)

      def execute(turtle: Turtle): Unit =
        this match {
          case Drawing.Sequential(left, right) =>
            left.execute(turtle)
            right.execute(turtle)
          case _ => ???
        }
    }

    object Drawing {
      case object Blank                                          extends Drawing
      case object Draw                                           extends Drawing
      case object GoForward                                      extends Drawing
      case object GoBackward                                     extends Drawing
      final case class Sequential(left: Drawing, right: Drawing) extends Drawing
      final case class TurnLeft(degrees: Int)                    extends Drawing
      final case class TurnRight(degrees: Int)                   extends Drawing

      val blank: Drawing = Blank
      val draw: Drawing  = Draw

      def goForward: Drawing               = GoForward
      def goBackward: Drawing              = GoBackward
      def turnLeft(degrees: Int): Drawing  = TurnLeft(degrees)
      def turnRight(degrees: Int): Drawing = TurnRight(degrees)
    }
  }

}
