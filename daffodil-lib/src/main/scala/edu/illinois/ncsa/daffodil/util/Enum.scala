package edu.illinois.ncsa.daffodil.util
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Enum Idiom
 *
 * Note that we looked online at Enum idioms. We ended up having all sorts
 * of subtle initialization errors.
 *
 * We ended up back at just case objects inside a packaging object.
 *
 * About all this accomplishes is to insist that this is how the enums work.
 * <pre>
 * object Suits extends Enum {
 *   abstract sealed trait Type extends EnumValueType
 *   case object Clubs extends Type
 *   case object Hearts extends Type
 *   ...
 *   }
 *
 * ...
 *   def getClubs: Suits.Type == Suits.Clubs
 *   // must use Suits.Type to refer to the type.
 *   // and use Suites.Clubs to refer to an enum value.
 * </pre>
 *
 * If you want ordered, then make Type extend Ordered[Type] add an
 * integer id field, etc.
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

  type Type <: EnumValueType

  // Base class for enum values
  protected trait EnumValueType

}
