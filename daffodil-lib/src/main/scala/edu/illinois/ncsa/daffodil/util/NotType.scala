package edu.illinois.ncsa.daffodil.util

import scala.language.higherKinds

object notType {

  /**
   * See stack overflow:
   * https://groups.google.com/forum/#!topic/scala-language/4SQt-n1l9Zk
   * or Gist
   * https://gist.github.com/milessabin/c9f8befa932d98dcc7a4
   */
  // Encoding for "A is not a subtype of B"
  trait NotSubtype[A, B]

  // Uses ambiguity to rule out the cases we're trying to exclude
  implicit def rule1_A_should_not_be_subtype_of_B[A, B]: NotSubtype[A, B] = null
  implicit def rule2_A_should_not_be_subtype_of_B[A, B >: A]: NotSubtype[A, B] = null
  implicit def rule3_A_should_not_be_subtype_of_B[A, B >: A]: NotSubtype[A, B] = null

  // Type alias for context bound
  type Not[NR] = {
    type Subtype[U] = NotSubtype[U, NR]
  }
}

private[util] class ExampleOfTypeExclusion {

  import notType._ // need both the implicits and the Not type definition

  def exampleFunc[T: Not[Unit]#Subtype](t: T) = t // does not accept T of type Unit

  def exampleUnit: Unit = ???
  // def exampleCall1 = exampleFunc(exampleUnit) // fails to compile. Poor error message, but fails.
  def exampleCall2 = exampleFunc(5) // ok

  // class type parameter is not a Throwable
  class Example[T: Not[Throwable]#Subtype]

  // val example1 = new Example[Exception] // fails to compile. Nothing like a message saying "can't be Throwable" tho.
  val example2 = new Example[String] // ok

}
