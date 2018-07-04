//package lambdaconf.introfp

import scala.language.higherKinds

/**
  * Q: Why generalize?
  * A: The smallest amount of known structure in a function body is optimal for avoiding implementation errors.
  *
  * Q: Parametric Polymorphism vs. Type Classes?
  * A: PP is throwing away structure, while TC is gaining structure
  *
  * Q: How to structure type class instances?
  * A: If you own the type, then in the companion object of that type, otherwise, in the companion object of the type class
  */
/**
  * Pillars:
  * 1. Totality
  * 2. Determinism
  * 3. Purity
  * 4. Parametric polymorphism
  *  - methods are not functions in Scala
  *  - simulated in Scala via Object.apply
  *
  * 5. Product/Sum types
  * - case classes are products
  * - sealed traits are used to represent Sum types
  *
  * 6. Empty sets of type A could only be `absurd`: final abstract class Zero { def absurd[A]: A }
  * - they are sometimes needed to describe a computation that can never be reached, but is still compiler-safe
  *
  * 7. Representing application through types
  * - enables impossible states being unrepresentable
  *
  * 8. Higher kinds are NOT standalone types, but rather type constructor
  * - a type must definer a set of values, so a type constructor without a type parameter doesn't conform to that law.
  * - in other words, a higher kind is a function, which expects types, and returns a type constructor
  *
  * The notion is as follows:
  * A kind accepts an asterisk `*` and returns a value function
  * A higher kind accepts a kind constructor [* => *] and returns a value function
  *
  * Examples:
  * {-- List is a kind function [* => *]          --}
  * {-- List[A] is a value function [*] => value  --}
  *
  * trait method                *
  * trait method[F]             * =>   *
  * trait method[F[_]]         (* => *) => *
  *
  * **
  *  - value level function:
  *   case class Person(age: Int)
  *  - type level function (takes a type and returns a value level function)
  *   case class Person[A](age: A)
  * **
  *
  * 9. Partial application in Scala
  *
  * - Given a non-curried function, we can still curry it with an underscore:
  * def add(Int, Int): Int
  * can be partially applied:
  * val add1 = add(1, _)
  *
  *
  *
  * - Type level partial application:
  * - Use case: Convert F[_, _] to F[_] (example: convert Map[K, N] to MapK[N])
  * is implemented with "type lambda"s:
  * Syntax: ({type MapK[A] = Map[K, A]}#MapK)
  * # create a type alias with the right number of unknowns
  * # wrap with curly braces
  * # add a hash tag with the projected type suffix
  * # wrap all with parentheses
  *
  * OR: use "kind projection"
  * Map[K, ?] which makes it basically the same as ({type MapK[A] = Map[K, A]}#MapK)
  *
  * 10. Type Classes
  *
  *
  */
object introfp {
  type Parser[E, A] = String => Either[E, (String, A)]

  def alt[E, A](l: Parser[E, A], r: Parser[E, A]): Parser[E, A] =
    (str: String) => l(str).fold(_ => r(str), Right(_))

  object second {
    def apply[B, A](tu: (B, A)): A = tu._2
  }

  second((true, "false"))

  final abstract class Zero {
    def absurd[A]: A
  }

  def simplify[A](e: Either[A, Zero]): A = e.fold(identity, _.absurd)

  // Type expansion using asterisk notion

  /*
    Map // _[_, _] :: [*, *] => *
    Option // _[_] :: [*] => *
    Either // _[_,_] :: [*, *] => *
    Future // _[_] :: [*] => *
    RDD // _[_] :: [*] => *
    Tuple3 // _[_, _, _] :: [*, *, *] => *
    CanBuildFrom // _[_, _, _] :: [*, *] => *
   */

  /*trait Ex1[_] // * => *
  trait Ex2[_, _] // [*, *] => *
  trait Ex3[_[_], _] // [* => *, *] => *
  trait Ex4[_[_[_]]] // [(* => *) => *] => **/

  // Partial type application

  trait Sized[F[_]] { // takes 1 type parameter
    def size[A](fa: F[A]): Int
  }

  // Map takes 2 type parameters, so we eliminate one and apply the method for the second
  def SizedMap[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    def size[A](ma: Map[K, A]): Int = ma.size
  }

  object type_classes_usage {
    import type_classes._
    def appendString(l: String, r: String): String = l <> r

    def appendInt(l: Int, r: Int) = l <> r
  }

  object type_classes {

    trait Semigroup[A] {
      def append(l: A, r: A): A
    }

    implicit val SemigroupString: Semigroup[String] = new Semigroup[String] {
      def append(l: String, r: String): String = l + r
    }

    implicit class SemigroupSyntax[A](a: A) {
      def <>(r: A)(implicit S: Semigroup[A]): A =
        S.append(a, r)
    }

    implicit val SemigroupInt: Semigroup[Int] = new Semigroup[Int] {
      def append(l: Int, r: Int): Int = l * r
    }

    implicit def SemigroupList[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
      def append(l: List[A], r: List[A]): List[A] = l ++ r
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    object Monoid {
      def apply[A](implicit M: Monoid[A]): Monoid[A] = M
    }

    def empty[A: Monoid] = Monoid[A].empty
  }
}
