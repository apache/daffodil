package edu.illinois.ncsa.daffodil.util

/**
 * Delay[T]
 *
 * This Delayed evaluation technique is an alternative to staggered
 * multi-stage factories, or currying. Turns out currying works fine in
 * Scala for functions, but not for class constructors, so it's easier to
 * make the class constructor take the usual bunch of arguments, but the
 * ones we might have wanted to supply later, we instead supply them, but
 * as Delay objects.
 *
 * For more info, this is used in the IncludeImport stuff in DSOM.
 */

class Delay[T] private (v: => T) {
  lazy val value = v
  private val hasValue: Boolean = false
  override def toString = {
    val bodyString = if (hasValue) value.toString else "..."
    "Delay(" + bodyString + ")"
  }
}

object Delay {
  def apply[T](v: => T) = new Delay(v)
}
