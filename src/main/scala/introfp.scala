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
  * A way to define common behavior without resorting to OOP inheritance
  * - define a trait with abstract common behavior (laws)
  * - define a companion object
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
      /*
         Associativity Law:
         (l append r) append z = l append (r append z)
       */
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
      /*
        1. Is a Semigroup (associativity)
        2. Left identity
            Monoid[A].empty + x = x
        3. Right identity
            x + Monoid[A].empty = x
       */
      def empty: A
    }

    object Monoid {
      def apply[A](implicit M: Monoid[A]): Monoid[A] = M
    }

    def empty[A: Monoid] = Monoid[A].empty

    implicit def MapSemigroup[K, V: Semigroup]: Semigroup[Map[K, V]] =
      new Semigroup[Map[K, V]] {
        def append(l: Map[K, V], r: Map[K, V]): Map[K, V] = {
          val combined = for {
            (leftK, leftV) <- l
            (rightK, rightV) <- r
            if leftK == rightK
          } yield (leftK, leftV <> rightV)

          l ++ r ++ combined
        }
      }
  }

  object functor {

    trait Functor[F[_]] {
      /*
        1. Identity Law:
            fmap(fa, identity) = fa
        2. Composition Law:
            fmap(fmap(fa, g), g) == fmap(fa, f.compose(g))
       */
      def fmap[A, B](fa: F[A], f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

      implicit val ListFunctor: Functor[List] = new Functor[List] {
        def fmap[A, B](fl: List[A], f: A => B): List[B] = fl map f
      }

      implicit val OptionFunctor: Functor[Option] = new Functor[Option] {
        def fmap[A, B](fa: Option[A], f: A => B): Option[B] = fa match {
          case None    => None
          case Some(v) => Some(f(v))
        }
      }

      implicit def FunctionFunctor[K]: Functor[Function[K, ?]] =
        new Functor[Function[K, ?]] {
          def fmap[A, B](fa: K => A, f: A => B): K => B = fa andThen f
        }
    }

    implicit class FunctorSyntax[F[_], A](fa: F[A]) {
      def map[B](f: A => B)(implicit F: Functor[F]): F[B] = F fmap (fa, f)
    }

    trait Contravariant[F[_]] {
      def contramap[A, B](fa: F[A], f: B => A): F[B]
    }

    implicit def ContravariantFunction[K]: Contravariant[Function[?, K]] =
      new Contravariant[Function[?, K]] {
        def contramap[A, B](fa: A => K, f: B => A): B => K = f andThen fa
      }

    trait Invariant[F[_]] { // Invariant Functor
      def xmap[A, B](fa: F[A], ab: A => B, ba: B => A): F[B]
    }

    trait Apply[F[_]] extends Functor[F] {
      def ap[A, B](ff: F[A => B], fa: F[A]): F[B]

      def zap[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        ap(fmap(fa, (a: A) => (b: B) => (a, b)), fb)
    }

    implicit val OptionApply: Apply[Option] = new Apply[Option] {
      def fmap[A, B](fa: Option[A], f: A => B): Option[B] = fa map f
      def ap[A, B](ff: Option[A => B], fa: Option[A]): Option[B] =
        (ff, fa) match {
          case (Some(f), Some(a)) => Some(f(a))
          case _                  => None
        }
    }

    implicit val ListApply: Apply[List] = new Apply[List] {
      def fmap[A, B](fa: List[A], f: A => B): List[B] = fa map f
      def ap[A, B](ff: List[A => B], fa: List[A]): List[B] =
        for {
          f <- ff
          a <- fa
        } yield f(a)
    }

    case class Parser[E, A](run: String => Either[E, (String, A)])

    implicit def ParserApply[E]: Apply[Parser[E, ?]] = new Apply[Parser[E, ?]] {
      def ap[A, B](ff: Parser[E, A => B], fa: Parser[E, A]): Parser[E, B] =
        Parser[E, B](i =>
          ff.run(i) match {
            case Left(e) => Left(e)
            case Right((i, f)) =>
              fa.run(i) match {
                case Left(e) => Left(e)
                case Right((i, a)) =>
                  Right((i, f(a)))
              }
        })
      def fmap[A, B](fa: Parser[E, A], f: A => B): Parser[E, B] =
        Parser[E, B](i =>
          fa.run(i) match {
            case Left(e)       => Left(e)
            case Right((i, a)) => Right((i, f(a)))
        })
    }

    trait Applicative[F[_]] extends Apply[F] {
      def point[A](a: A): F[A]
    }

    trait Monad[F[_]] extends Applicative[F] {
      def bind[A, B](fa: F[A], afb: A => F[B]): F[B]
    }

    object Monad {
      def apply[F[_], A](fa: F[A])(implicit F: Monad[F]): Monad[F] = F

      implicit val MonadOption: Monad[Option] = new Monad[Option] {
        def point[A](a: A): Option[A] = Some(a)
        def fmap[A, B](fa: Option[A], fab: A => B): Option[B] =
          fa match {
            case None    => None
            case Some(v) => Some(fab(v))
          }
        def ap[A, B](fab: Option[A => B], fa: Option[A]): Option[B] = ???
        def bind[A, B](fa: Option[A], afb: A => Option[B]): Option[B] =
          fa match {
            case None    => None
            case Some(v) => afb(v)
          }
      }
      //TODO: complete
//      implicit def MonadParser[E]: Monad[Parser[E, ?]] =
//        new Monad[Parser[E, ?]] {
//          def point[A](a: A): Parser[E, A] =
//            Parser((str: String) => Right((str, a)))
//          def fmap[A, B](fa: Parser[E, A], fab: A => B): Parser[E, B] = ???
//          def ap[A, B](fab: Option[A => B], fa: Option[A]): Parser[E, B] = ???
//          def bind[A, B](fa: Option[A], afb: A => Option[B]): Option[B] =
//            fa match {
//              case None    => None
//              case Some(v) => afb(v)
//            }
//        }
    }
  }

  object effects {
    import functor._
    import type_classes._

    case class IO[E, A](unsafePerformIO: () => Either[E, A]) { self =>
      def map[B](f: A => B): IO[E, B] =
        IO[E, B](() => self.unsafePerformIO().map(f))
      def flatMap[B](fb: A => IO[E, B]): IO[E, B] =
        IO[E, B](() =>
          self.unsafePerformIO() match {
            case Left(e)  => Left(e)
            case Right(a) => fb(a).unsafePerformIO()
        })
      def attempt: IO[Void, Either[E, A]] =
        IO[Void, Either[E, A]](() => Right(self.unsafePerformIO()))
    }

    object IO {
      def point[E, A](a: A): IO[E, A] = IO[E, A](() => Right(a))
      def fail[E, A](e: E): IO[E, A] = IO[E, A](() => Left(e))

      implicit def MonadIO[E]: Monad[IO[E, ?]] = new Monad[IO[E, ?]] {
        def point[A](a: A): IO[E, A] = IO.point(a)
        def fmap[A, B](fa: IO[E, A], f: A => B): IO[E, B] = fa.map(f)
        def ap[A, B](ff: IO[E, A => B], fa: IO[E, A]): IO[E, B] =
          for {
            f <- ff
            a <- fa
          } yield f(a)
        def bind[A, B](fa: IO[E, A], f: A => IO[E, B]): IO[E, B] =
          fa.flatMap(f)
      }
    }

    object console {
      // impure
      def run: Unit = {
        println("Name?")
        val name = scala.io.StdIn.readLine()
        println("Hi " + name)
      }

      // pure
      def putStrLn(line: String): IO[Void, Unit] =
        IO(() => Right(println(line)))

      def getStrLn: IO[Void, String] =
        IO(() => Right(scala.io.StdIn.readLine()))
    }
  }

}
