package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.xml.XMLEvent
import edu.illinois.ncsa.daffodil.xml.XMLEventCursor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._

/**
 * various diagnostic situations associated with the incoming XML for creating and
 * infoset.
 *
 * Note: You cannot save the xmlEvent in a data structure, it is part of an accessor/cursor.
 */
object InvalidInfosetXML {

  private def err(info: ElementRuntimeData, xmlCursor: XMLEventCursor, event: XMLEvent, str: String, args: Any*) = {
    UnparseError(One(info.schemaFileLocation), Nope, str.format(args: _*))
  }

  def textInElementOnlyContent(info: ElementRuntimeData, xmlCursor: XMLEventCursor, accessorXMLEvent: XMLEvent): Nothing =
    err(info, xmlCursor, accessorXMLEvent, "Non-whitespace text found in element-only context: '%s'", accessorXMLEvent)

  def elementFoundInSimpleContent(parent: ElementRuntimeData, sibling: ElementRuntimeData, xmlCursor: XMLEventCursor,  accessorXMLEvent: XMLEvent): Nothing = {
    err(parent, xmlCursor, accessorXMLEvent, "Sibling element found as child in simple content: '%s'", sibling)
  }
}
