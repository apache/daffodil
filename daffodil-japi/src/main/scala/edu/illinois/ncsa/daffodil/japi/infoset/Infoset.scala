/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.japi.infoset

import edu.illinois.ncsa.daffodil.infoset.{ InfosetOutputter => SInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ InfosetInputter => SInfosetInputter }
import edu.illinois.ncsa.daffodil.infoset.{ ScalaXMLInfosetOutputter => SScalaXMLInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ XMLTextInfosetOutputter => SXMLTextInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ JsonInfosetOutputter => SJsonInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ NullInfosetOutputter => SNullInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ JDOMInfosetOutputter => SJDOMInfosetOutputter }
import edu.illinois.ncsa.daffodil.infoset.{ ScalaXMLInfosetInputter => SScalaXMLInfosetInputter }
import edu.illinois.ncsa.daffodil.infoset.{ XMLTextInfosetInputter => SXMLTextInfosetInputter }
import edu.illinois.ncsa.daffodil.infoset.{ JsonInfosetInputter => SJsonInfosetInputter }
import edu.illinois.ncsa.daffodil.infoset.{ JDOMInfosetInputter => SJDOMInfosetInputter }
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import edu.illinois.ncsa.daffodil.infoset.InfosetInputterEventType
// TODO: Not sure about the access to internal infoset implementation details.
// Should API users have this deep access to our internal infoset?
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.infoset.DIComplex
import edu.illinois.ncsa.daffodil.infoset.DIArray

/**
 * Abstract class used to determine how the infoset representation should be
 * input from a call to DataProcessor#unparse. This uses a Cursor API, such
 * that each call to advance/inspect must update a cursor value, minimizing
 * allocations. Callers of advance/inspect are expected to copy out any
 * information from advanceAccessor and inspectAccessor if they need to retain
 * the information after a call to advance/inspect.
 **/
abstract class InfosetInputter extends SInfosetInputter {
  /**
   * Return the current infoset inputter event type
   */
  def getEventType(): InfosetInputterEventType 

  /**
   * Get the local name of the current event. This will only be called when the
   * current event type is StartElement.
   */
  def getLocalName(): String

  /**
   * Get the namespace of the current event. This will only be called when the
   * current event type is StartElement. If the InfosetInputter does not
   * support namespaces, this shoud return null. This may return null to
   * represent no namespaces.
   */
  def getNamespaceURI(): String

  /**
   * Get the content of a simple type. This will only be called when the
   * current event type is StartElement and the element is a simple type. If
   * the event contains complex data, it is an error and should throw
   * NonTextFoundInSimpleContentException. If the element does not have any
   * simple content, this should return either null or the empty string.
   */
  def getSimpleText(): String

  /**
   * Determine if the current event is nilled. This will only be called when
   * the current event type is StartElement. Return MaybeBoolean.Nope if no
   * nil property is set, which implies the element is not nilled. Return
   * MaybeBoolean(false) if the nil property is set, but it is set to false.
   * Return MaybeBoolean(true) if the nil property is set to true.
   */
  def isNilled(): MaybeBoolean

  /**
   * Return true if there are remaining events. False otherwise.
   */
  def hasNext(): Boolean

  /**
   * Move the internal state to the next event.
   */
  def next(): Unit
}

/**
 * Abstract class used to determine how the infoset representation should be
 * output from a call to DataProcessor#parse. The Daffodil core will call
 * the various methods of this class in an order appropriate to create an
 * infoset representation.
 *
 * Classes that extend InfosetOutputter are not guaranteed to be thread-safe.
 **/
abstract class InfosetOutputter extends SInfosetOutputter {

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  def reset(): Unit // call to reuse these. When first constructed no reset call is necessary.

  /**
   * Called by Daffodil internals to signify the beginning of the infoset.
   *
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startDocument(): Boolean

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   *
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endDocument(): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */

  def startSimple(diSimple: DISimple): Boolean

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endSimple(diSimple: DISimple): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   *
   * @param diComplex the complex element that is started. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startComplex(diComplex: DIComplex): Boolean

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   *
   * @param diComplex the complex element that is ended. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endComplex(diComplex: DIComplex): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   *
   * @param diArray the array that is started. Various fields of
   *                DIArray can be accessed to determine things like the
   *                name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startArray(diArray: DIArray): Boolean

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   *
   * @param diArray the array that is ended. Various fields of
   *                DIArray can be accessed to determine things like the
   *                name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endArray(diArray: DIArray): Boolean
}

/**
 * The below classes are all just proxies of the classes implemented in the
 * core daffodil code. The reason for this is purely for documentation. When
 * only generate javadoc on the daffodil-japi subproject. By having this proxy
 * classes, we can document these classes and have a small and clean javadoc.
 */

 
/**
 * Output the infoset as a scala.xml.Node
 *
 * @param showFormatInfo add additional properties to each scala.xml.Node for debug purposes
 */
class ScalaXMLInfosetOutputter(showFormatInfo: Boolean = false)
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SScalaXMLInfosetOutputter(showFormatInfo)

  def getResult(): scala.xml.Node = infosetOutputter.getResult()
}

/**
 * Output the infoset as XML Text, written to a java.io.Writer
 *
 * @param writer the java.io.Writer to write the XML text to
 * @param pretty enable or disable pretty printing. Pretty printing will only
 *               inserts indentation and newlines where it will not affect the
 *               content of the XML.
 */
class XMLTextInfosetOutputter(writer: java.io.Writer, pretty: Boolean = true)
  extends InfosetOutputterProxy {
  
  override val infosetOutputter = new SXMLTextInfosetOutputter(writer, pretty)
}

/**
 * Output the infoset as json text, written to a java.io.Writer
 *
 * @param writer the java.io.Writer to write the json text to
 * @param pretty enable or disable pretty printing. Pretty printing will only
 *               inserts indentation and newlines where it will not affect the
 *               content of the json.
 */
class JsonInfosetOutputter(writer: java.io.Writer, pretty: Boolean = true)
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SJsonInfosetOutputter(writer, pretty)  
}

/**
 * Output the infoset as a jdom Document
 */
class JDOMInfosetOutputter()
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SJDOMInfosetOutputter()

  def getResult(): org.jdom2.Document = infosetOutputter.getResult()
}

/**
 * Ignore all infoset output
 */
class NullInfosetOutputter()
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SNullInfosetOutputter()
}


/**
 * Read in an infoset in the form of a scala.xml.Node
 *
 * @param node the infoset in the form of a scala.xml.Node
 */
class ScalaXMLInfosetInputter(node: scala.xml.Node)
  extends InfosetInputterProxy {

  override val infosetInputter = new SScalaXMLInfosetInputter(node)
}

/**
 * Read in an infoset in the form of XML text from a java.io.Reader
 *
 * @param reader the java.io.Reader to read the XML text from
 */
class XMLTextInfosetInputter(reader: java.io.Reader)
  extends InfosetInputterProxy {
  
  override val infosetInputter = new SXMLTextInfosetInputter(reader)
}

/**
 * Read in an infoset in the form of json text from a java.io.Reader
 *
 * @param reader the java.io.Reader to read the json text from
 */
class JsonInfosetInputter(reader: java.io.Reader)
  extends InfosetInputterProxy {

  override val infosetInputter = new SJsonInfosetInputter(reader)
}

/**
 * Read in an infoset in the form of jdom2 Document
 *
 * @param document the infoset in the form of a jdom2 Document
 */
class JDOMInfosetInputter(document: org.jdom2.Document)
  extends InfosetInputterProxy {

  override val infosetInputter = new SJDOMInfosetInputter(document)
}

/* A proxy for existing infoset inputters */
abstract class InfosetInputterProxy extends InfosetInputter {

  protected val infosetInputter: SInfosetInputter

  override def getEventType() = infosetInputter.getEventType()
  override def getLocalName() = infosetInputter.getLocalName()
  override def getNamespaceURI() = infosetInputter.getNamespaceURI()
  override def getSimpleText() = infosetInputter.getSimpleText()
  override def hasNext() = infosetInputter.hasNext()
  override def isNilled() = infosetInputter.isNilled()
  override def next() = infosetInputter.next()
  override lazy val supportsNamespaces = infosetInputter.supportsNamespaces

  override def fini = infosetInputter.fini
}

/* A proxy for existing infoset outputters */
abstract class InfosetOutputterProxy extends InfosetOutputter {

  protected val infosetOutputter: SInfosetOutputter

  override def reset(): Unit = infosetOutputter.reset()
  override def startDocument(): Boolean = infosetOutputter.startDocument()
  override def endDocument(): Boolean = infosetOutputter.endDocument()
  override def startSimple(diSimple: DISimple): Boolean = infosetOutputter.startSimple(diSimple)
  override def endSimple(diSimple: DISimple): Boolean = infosetOutputter.endSimple(diSimple)
  override def startComplex(diComplex: DIComplex): Boolean = infosetOutputter.startComplex(diComplex)
  override def endComplex(diComplex: DIComplex): Boolean = infosetOutputter.endComplex(diComplex)
  override def startArray(diArray: DIArray): Boolean = infosetOutputter.startArray(diArray)
  override def endArray(diArray: DIArray): Boolean = infosetOutputter.endArray(diArray)
}
