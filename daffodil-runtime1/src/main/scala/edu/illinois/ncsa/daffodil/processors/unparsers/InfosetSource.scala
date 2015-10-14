package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.util.IteratorWithPeek

class InfosetError(kind: String, args: String*) extends ProcessingError("Infoset Error", Nope, Nope, kind, args: _*)

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
    val infosrc = new UnparserAugmentingInfosetSource(erd, src)
    infosrc
  }

  /**
   * Without constructing the intermediate XML Tree, that is, on the fly, generates the
   * DFDL Infoset events for the InfosetSource as they are pulled.
   *
   * This is schema aware as it must infer when arrays are starting and ending
   * in order to generate those events as well as the startElement endElement events.
   */
  def fromXMLSource(xmlEventReader: Iterator[scala.xml.pull.XMLEvent], rootElementERD: ElementRuntimeData): InfosetSource = {
    val orig = new InfosetSourceFromXMLEventReader(xmlEventReader, rootElementERD)
    val infosrc = new UnparserAugmentingInfosetSource(rootElementERD, orig)
    infosrc
  }

}

trait InfosetSource extends IteratorWithPeek[InfosetEvent]

sealed abstract class InfosetEvent(val node: DINode) {
  def namedQName = node.namedQName
  def erd: ElementRuntimeData = node match {
    case a: DIArray => a.parent.runtimeData
    case e: DIElement => e.runtimeData
  }
}

/**
 * TODO: PERFORMANCE: Why make these objects at all. Why not make them just be event call-backs so as to
 * completely avoid allocating these little crud objects? A flat stream of these events has no structure
 * that a set of flat callbacks doesn't convey.
 */
case class End(override val node: DINode) extends InfosetEvent(node)
case class Start(override val node: DINode) extends InfosetEvent(node)
