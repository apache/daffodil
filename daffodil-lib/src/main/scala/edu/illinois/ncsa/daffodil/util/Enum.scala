package edu.illinois.ncsa.daffodil.util
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Enum Idiom
 *
 * From StackOverflow: http://stackoverflow.com/questions/14947179/using-a-custom-enum-in-the-scala-worksheet-i-am-receiving-an-error-java-lang-ex
 * thanks to Rex Kerr and Chaotic3quilibrium (aka Jim O'Flaherty, Jr.)
 *
 * How to use: search the source for " extends Enum" and you'll find a few examples.
 * One of these, the one in Logger.scala's LogLevel object enum, is setup to
 * work across the Java API (japi), so that's an example of how to achieve
 * a scala enum, but also have it be usable across from Java calling into scala.
 *
 * Note: There is a different enum idiom used in the DFDL properties code
 * that is laid down by the code generator. This one is a small improvement
 * on that one, but no point in changing that one and modifying the code generator
 * and all, until Scala has a language-supported enum idiom (which it will
 * some day, as it is being discussed anyway.)
 */
abstract class Enum {

  type Type <: EnumVal

  protected var nextId: Int = 0

  private var values_ = List[Type]()
  private var valuesById_ = Map[Int, Type]()
  private var valuesByName_ = Map[String, Type]()

  def values = values_
  def valuesById = valuesById_
  def valuesByName = valuesByName_

  def apply(id: Int) = valuesById.get(id) // Some|None
  def apply(name: String) = valuesByName.get(name) // Some|None

  // Specifically for use by Java code
  def enumByName(name: String): Type = valuesByName.get(name).getOrElse(Assert.abort("no enum value with that name."))
  def idByName(name: String): Int = enumByName(name).id

  // Base class for enum values; it registers the value with the Enum.
  protected abstract class EnumVal extends Ordered[Type] {
    val theVal = this.asInstanceOf[Type] // only extend EnumVal to Val
    val id = nextId
    def instance = theVal
    def bumpId { nextId += 1 }
    def compare(that: Type) = this.id - that.id
    def init { // <--------------------------changed name from apply
      if (valuesById_.get(id) != None)
        throw new Exception("cannot init " + this + " enum value twice")
      bumpId
      values_ ++= List(theVal)
      valuesById_ += (id -> theVal)
      valuesByName_ += (toString -> theVal)
    }
  }
}