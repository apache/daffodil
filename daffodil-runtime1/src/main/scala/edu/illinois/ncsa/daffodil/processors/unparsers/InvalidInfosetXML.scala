package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLEvent
import edu.illinois.ncsa.daffodil.xml.XMLElementEvent
import edu.illinois.ncsa.daffodil.xml.XMLEventCursor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

/**
 * various diagnostic situations associated with the incoming XML for creating and
 * infoset.
 *
 * Note: You cannot save the xmlEvent in a data structure, it is part of an accessor/cursor.
 */
object InvalidInfosetXML {

  private def err(info: DPathElementCompileInfo, xmlCursor: XMLEventCursor, event: XMLEvent, str: String, args: Any*) = {
    val errStr = xmlCursor.getXMLErrorInfo(event.pos, str.format(args: _*))
    UnparseError(One(info.schemaFileLocation), Nope, errStr)
  }

  def textInElementOnlyContent(info: DPathElementCompileInfo, xmlCursor: XMLEventCursor, accessorXMLEvent: XMLEvent): Nothing = err(info, xmlCursor, accessorXMLEvent, "Non-whitespace text found in element-only context: '%s'", accessorXMLEvent)
  def xsiNilPrefixUnbound(info: DPathElementCompileInfo, xmlCursor: XMLEventCursor, accessorXMLEvent: XMLEvent): Nothing =
    err(info, xmlCursor, accessorXMLEvent, "xsi:nil='true' found, but the prefix 'xsi' is not associated with a namespace.\n" +
      "it should have namespace " + XMLUtils.XSI_NAMESPACE.toString)
  def xsiNilPrefixIncorrect(info: DPathElementCompileInfo, xmlCursor: XMLEventCursor, accessorXMLEvent: XMLEvent, xsiNS: String): Nothing =
    err(info, xmlCursor, accessorXMLEvent, "xsi:nil='true' found, but the prefix 'xsi' is bound to %s.\n" +
      "It should have namespace " + XMLUtils.XSI_NAMESPACE.toString, xsiNS)
  def endTagNotForExpectedElement(eev: XMLElementEvent, nodeERD: ElementRuntimeData, thisERD: ElementRuntimeData): Nothing = ???
  def elementFoundInSimpleContent(accessorXMLEvent: XMLEvent): Nothing = ???
}
