package fpfinal.common

import cats.Order
import cats.data.Validated.Invalid
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
    if (s.length <= n) Validated.Valid(s)
    else Invalid(NonEmptyChain.one("string length exceed limit - lenght: $n - string: $s"))


  /**
    * TODO #2: Turn this String into a validated double
    */
  def double(s: String): IsValid[Double] =
    ???

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
