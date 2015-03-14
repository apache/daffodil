package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.StepQName
import edu.illinois.ncsa.daffodil.xml.QNameBase
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.NamedQName

/**
 * The schema compiler computes this for each element.
 *
 * This is for use assembling the Daffodil Infoset from an XML representation.
 *
 */

trait NextElementResolver extends Serializable {
  def nextElement(name: String, nameSpace: String): Maybe[ElementRuntimeData]
}

class NoNextElement(schemaFileLocation: SchemaFileLocation) extends NextElementResolver {

  override def nextElement(local: String, namespace: String): Maybe[ElementRuntimeData] = Nope
}

class OnlyOnePossibilityForNextElement(schemaFileLocation: SchemaFileLocation, nextERD: ElementRuntimeData)
  extends NextElementResolver {
  override def nextElement(local: String, namespace: String): Maybe[ElementRuntimeData] = {
    val nqn = nextERD.namedQName
    val sqn = StepQName(None, local, NS(namespace))
    if (!sqn.matches(nqn)) {
      UnparseError(One(schemaFileLocation), Nope, "Next element must be %s. But found %s", nqn, sqn)
    }
    One(nextERD)
  }
}

/**
 * Schema compiler computes the map here, and then attaches this object to the
 * ERD of each element.
 */
class SeveralPossibilitiesForNextElement(loc: SchemaFileLocation, nextERDMap: Map[QNameBase, ElementRuntimeData])
  extends NextElementResolver {
  Assert.usage(nextERDMap.size > 1, "should be more than one mapping")

  /**
   * Annoying, but scala's immutable Map is not covariant in its first argument
   * the way one would normally expect a collection to be.
   *
   * So Map[StepQName, ElementRuntimeData] is not a subtype of Map[QNameBase, ElementRuntimeData]
   * which means when we construct a Map using the NamedQName of the elements,
   * we can't use that with StepQNames as the query items. But QName comparisons
   * are carefully strongly typed to prevent you from comparing the wrong kinds.
   * For example, you can check if a StepQName matches a NamedQName, but you can't compare
   * two NamedQNames together (because, generally, that would be a mistake.)
   *
   * So we need a cast upward to QNameBase
   */
  override def nextElement(local: String, namespace: String): Maybe[ElementRuntimeData] = {
    val sqn = StepQName(None, local, NS(namespace)) // these will match in a hash table of NamedQNames.
    val optNextERD = nextERDMap.get(sqn.asInstanceOf[QNameBase])
    val res = optNextERD.getOrElse {
      val keys = nextERDMap.keys.toSeq
      UnparseError(One(loc), Nope, "Found %s, but next element must be one of %s.", sqn, keys.mkString("\n"))
    }
    One(res)
  }
}