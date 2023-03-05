package fpfinal.common

import cats.Order
import cats.data.{NonEmptyChain, NonEmptySet, Validated}
import fpfinal.app.Configuration.IsValid

import scala.collection.immutable.SortedSet

/**
  * Set of simple validations that can be reused across the different models.
  */
object Validations {

  /**
    * TODO #1: Check that this String's length does not exceed the provided limit.
    */
  def maxLength(s: String, n: Int): IsValid[String] =
//    if (s.length <= n) Validated.Valid(s)
//    else Validated.invalidNec(s"string length exceed limit - length: $n - string: $s")
    Validated.condNec(
      s.length <= n,
      s,
      s"string length exceed limit - length: $n - string: $s"
    )

  /**
    * TODO #2: Turn this String into a validated double
    */
  def double(s: String): IsValid[Double] = {
    // soluce 1
    //Validated.condNec(s.forall(_.isDigit), s.toDouble, s"cannot convert $s to double")
    // soluce 2
    //    s.toDoubleOption match {
    //      case Some(value) => Validated.valid(value)
    //      case None => Validated.invalidNec(s"cannot convert $s to double")
    //    }
    Validated.fromOption(
      s.toDoubleOption,
      NonEmptyChain(s"cannot convert $s to double")
    )
  }

  /**
    * Validates that a Double is >= 0
    */
  def nonNegative(x: Double): IsValid[Double] =
    Validated.condNec(x >= 0, x, s"Double should be nonnegative")

  /**
    * Validates that a list is non-empty and converts it to a NonEmptySet.
    */
  def nonEmptySet[A: Order](list: List[A]): IsValid[NonEmptySet[A]] =
    Validated.fromOption(
      NonEmptySet.fromSet(SortedSet.from(list)(Order[A].toOrdering)),
      NonEmptyChain("List should be non-empty")
    )

  /**
    * Validates that a String is non-empty.
    */
  def nonEmptyString(s: String): IsValid[String] =
    Validated.condNec(s.nonEmpty, s, "String should be non-empty")

  /**
    * Validates that a String only contains letters.
    */
  def allLetters(s: String): IsValid[String] =
    Validated.condNec(s.forall(_.isLetter), s, "String should be all letters")
}
