package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLEvent
import edu.illinois.ncsa.daffodil.xml.XMLElementEvent
import edu.illinois.ncsa.daffodil.xml.XMLEventCursor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

/**
 * various diagnostic situations associated with the incoming XML for creating and
 * infoset.
 *
 * Note: You cannot save the xmlEvent in a data structure, it is part of an accessor/cursor.
 */
object InvalidInfosetXML {

  private def err(info: ElementRuntimeData, xmlCursor: XMLEventCursor, event: XMLEvent, str: String, args: Any*) = {
    val errStr = xmlCursor.getXMLErrorInfo(event.pos, str.format(args: _*))
    UnparseError(One(info.schemaFileLocation), Nope, errStr)
  }

  def textInElementOnlyContent(info: ElementRuntimeData, xmlCursor: XMLEventCursor, accessorXMLEvent: XMLEvent): Nothing =
    err(info, xmlCursor, accessorXMLEvent, "Non-whitespace text found in element-only context: '%s'", accessorXMLEvent)
  def endTagNotForExpectedElement(eev: XMLElementEvent, nodeERD: ElementRuntimeData, thisERD: ElementRuntimeData): Nothing = ???
  def elementFoundInSimpleContent(accessorXMLEvent: XMLEvent): Nothing = ???
}
