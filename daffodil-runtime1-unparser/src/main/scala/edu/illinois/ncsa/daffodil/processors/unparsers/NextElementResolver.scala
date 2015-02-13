package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.StepQName
import edu.illinois.ncsa.daffodil.xml.QNameBase
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * The schema compiler computes this for each element.
 *
 * This is for use assembling the Daffodil Infoset from an XML representation.
 *
 */
trait NextElementResolver extends Serializable {
  def nextElement(name: String, nameSpace: String): ElementRuntimeData
}

class OnlyOnePossibilityForNextElement(erd: ElementRuntimeData, nextERD: ElementRuntimeData)
  extends NextElementResolver {
  def nextElement(local: String, namespace: String) = {
    val nqn = nextERD.namedQName
    val sqn = StepQName(None, local, NS(namespace))
    if (!sqn.matches(nqn)) {
      UnparseError(One(erd.schemaFileLocation), Nope, "Next element must be %s. But found %s", nqn, sqn)
    }
    nextERD
  }
}

/**
 * Schema compiler computes the map here, and then attaches this object to the
 * ERD of each element.
 */
class SeveralPossibilitiesForNextElement(erd: ElementRuntimeData, nextERDMap: Map[QNameBase, ElementRuntimeData])
  extends NextElementResolver {
  Assert.usage(nextERDMap.size > 1, "should be more than one mapping")

  def nextElement(local: String, namespace: String) = {
    val sqn = StepQName(None, local, NS(namespace)) // these will match in a hash table of NamedQNames.
    val optNextERD = nextERDMap.get(sqn)
    val res = optNextERD.getOrElse {
      val keys = nextERDMap.keys.toSeq
      UnparseError(One(erd.schemaFileLocation), Nope, "Found %s, but next element must be one of %s.", sqn, keys.mkString("\n"))
    }
    res
  }
}