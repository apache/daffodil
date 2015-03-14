package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.DIDocument
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

class InfosetError(kind: String, args: String*) extends ProcessingError("Infoset Error", Nope, Nope, kind, args)

object InfosetSource {

  def fromInfosetTree(infoset: InfosetDocument): InfosetSource = {
    val is = new InfosetSourceFromTree(infoset)
    is
  }

  /**
   * Incoming XML has no notion of array. So that must be inferred so that
   * Start/EndInfosetArray are generated.
   */
  def fromXMLNode(xmlDocument: scala.xml.Node, erd: ElementRuntimeData): InfosetSource = {
    val doc = Infoset.newDocument(Infoset.elem2Infoset(erd, xmlDocument))
    val src = fromInfosetTree(doc)
    src
  }

  /**
   * Without constructing the intermediate XML Tree, that is, on the fly, generates the
   * DFDL Infoset events for the InfosetSource as they are pulled.
   *
   * This is schema aware as it must infer when arrays are starting and ending
   * in order to generate those events as well as the startElement endElement events.
   */
  def fromXMLSource(xmlEventReader: Iterator[scala.xml.pull.XMLEvent], rootElementERD: ElementRuntimeData): InfosetSource = {
    new InfosetSourceFromXMLEventReader(xmlEventReader, rootElementERD)
  }

}

trait InfosetSource extends Iterator[InfosetEvent]

sealed abstract class InfosetEvent(val node: DINode)
case class End(override val node: DINode) extends InfosetEvent(node)
case class Start(override val node: DINode) extends InfosetEvent(node)

