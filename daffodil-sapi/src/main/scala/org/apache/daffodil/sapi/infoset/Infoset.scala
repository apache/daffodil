/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.sapi.infoset

import org.apache.daffodil.infoset.{ InfosetOutputter => SInfosetOutputter }
import org.apache.daffodil.infoset.{ InfosetInputter => SInfosetInputter }
import org.apache.daffodil.infoset.{ ScalaXMLInfosetOutputter => SScalaXMLInfosetOutputter }
import org.apache.daffodil.infoset.{ XMLTextInfosetOutputter => SXMLTextInfosetOutputter }
import org.apache.daffodil.infoset.{ JsonInfosetOutputter => SJsonInfosetOutputter }
import org.apache.daffodil.infoset.{ NullInfosetOutputter => SNullInfosetOutputter }
import org.apache.daffodil.infoset.{ JDOMInfosetOutputter => SJDOMInfosetOutputter }
import org.apache.daffodil.infoset.{ W3CDOMInfosetOutputter => SW3CDOMInfosetOutputter }
import org.apache.daffodil.infoset.{ ScalaXMLInfosetInputter => SScalaXMLInfosetInputter }
import org.apache.daffodil.infoset.{ XMLTextInfosetInputter => SXMLTextInfosetInputter }
import org.apache.daffodil.infoset.{ JsonInfosetInputter => SJsonInfosetInputter }
import org.apache.daffodil.infoset.{ JDOMInfosetInputter => SJDOMInfosetInputter }
import org.apache.daffodil.infoset.{ W3CDOMInfosetInputter => SW3CDOMInfosetInputter }
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.infoset.InfosetInputterEventType
// TODO: Not sure about the access to internal infoset implementation details.
// Should API users have this deep access to our internal infoset?
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.infoset.DIArray
import org.apache.daffodil.dpath.NodeInfo

/**
 * Abstract class used to determine how the infoset representation should be
 * input from a call to [[DataProcessor.unparse(input* DataProcessor.unparse]]. This uses a Cursor API, such
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
  def getSimpleText(primType: NodeInfo.Kind): String

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
 * output from a call to [[DataProcessor.parse(input:org\.apache\.daffodil* DataProcessor.parse]]. The Daffodil core will call
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
 * only generate scaladoc on the daffodil-sapi subproject. By having this proxy
 * classes, we can document these classes and have a small and clean scaladoc.
 */

 
/**
 * [[InfosetOutputter]] to build an infoset represented as a scala.xml.Node
 *
 * @param showFormatInfo add additional properties to each scala.xml.Node for debug purposes
 */
class ScalaXMLInfosetOutputter(showFormatInfo: Boolean = false)
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SScalaXMLInfosetOutputter(showFormatInfo)

  /**
   * Get the scala.xml.Node representing the infoset created during a parse
   *
   * This function shuld only be called if ParseResult.isError() returns false
   */
  def getResult(): scala.xml.Node = infosetOutputter.getResult()
}

/**
 * [[InfosetOutputter]] to build an infoset represented as XML written to a java.io.OutputStream
 */
class XMLTextInfosetOutputter private (outputter: SXMLTextInfosetOutputter)
  extends InfosetOutputterProxy {

  /**
   * Output the infoset as XML Text, written to a java.io.Writer
   *
   * @param writer the java.io.Writer to write the XML text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the XML.
   */
  @deprecated("This constructor is deprecated. Use XMLTextInfosetOutputter(java.io.OutputStream, Boolean) instead.", "2.4.0")
  def this(writer: java.io.Writer, pretty: Boolean) = this(new SXMLTextInfosetOutputter(writer, pretty))

  /**
   * Output the infoset as XML Text, written to a java.io.Writer
   *
   * @param writer the java.io.Writer to write the XML text to
   */
  @deprecated("This constructor is deprecated. Use XMLTextInfosetOutputter(java.io.OutputStream, Boolean) instead.", "2.4.0")
  def this(writer: java.io.Writer) = this(writer, true)

  /**
   * Output the infoset as XML Text, written to a java.io.OutputStream
   *
   * @param os the java.io.OutputStream to write the XML text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the XML.
   */
  def this(os: java.io.OutputStream, pretty: Boolean) = this(new SXMLTextInfosetOutputter(os, pretty))

  override val infosetOutputter = outputter
}

/**
 * [[InfosetOutputter]] to build an infoset represented as JSON written to a java.io.OutputStream
 */
class JsonInfosetOutputter private (outputter: SJsonInfosetOutputter)
  extends InfosetOutputterProxy {

  /**
   * Output the infoset as json text, written to a java.io.Writer
   *
   * @param writer the java.io.Writer to write the json text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the json.
   */
  @deprecated("This constructor is deprecated. Use JsonInfosetOutputter(java.io.OutputStream, Boolean) instead.", "2.4.0")
  def this(writer: java.io.Writer, pretty: Boolean) = this(new SJsonInfosetOutputter(writer, pretty))

  /**
   * Output the infoset as json text, written to a java.io.Writer
   *
   * @param writer the java.io.Writer to write the json text to
   */
  @deprecated("This constructor is deprecated. Use JsonInfosetOutputter(java.io.OutputStream, Boolean) instead.", "2.4.0")
  def this(writer: java.io.Writer) = this(writer, true)

  /**
   * Output the infoset as json text, written to a java.io.OutputStream
   *
   * @param os the java.io.OutputStream to write the json text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the json.
   */
  def this(os: java.io.OutputStream, pretty: Boolean) = this(new SJsonInfosetOutputter(os, pretty))

  override val infosetOutputter = outputter
}

/**
 * [[InfosetOutputter]] to build an infoset represented as an org.jdom2.Document
 */
class JDOMInfosetOutputter()
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SJDOMInfosetOutputter()

  /**
   * Get the jdom Document representing the infoset created during a parse
   *
   * This function shuld only be called if ParseResult.isError() returns false
   */
  def getResult(): org.jdom2.Document = infosetOutputter.getResult()
}

/**
 * [[InfosetOutputter]] to build an infoset represented as an org.w3c.dom.Document
 */
class W3CDOMInfosetOutputter()
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SW3CDOMInfosetOutputter()

  /**
   * Get the w3c Document representing the infoset created during a parse
   *
   * This function shuld only be called if ParseResult.isError() returns false
   */
  def getResult(): org.w3c.dom.Document = infosetOutputter.getResult()
}

/**
 * [[InfosetOutputter]] that does not build an infoset represention, ignoring
 * all [[InfosetOutputter]] events
 */
class NullInfosetOutputter()
  extends InfosetOutputterProxy {

  override val infosetOutputter = new SNullInfosetOutputter()
}


/**
 * [[InfosetInputter]] to read an infoset represented as a scala.xml.Node
 *
 * @param node the scala.xml.Node infoset
 */
class ScalaXMLInfosetInputter(node: scala.xml.Node)
  extends InfosetInputterProxy {

  override val infosetInputter = new SScalaXMLInfosetInputter(node)
}

/**
 * [[InfosetInputter]] to read an infoset represented as XML from a java.io.InputStream
 */
class XMLTextInfosetInputter private (inputter: SXMLTextInfosetInputter)
  extends InfosetInputterProxy {

  /**
   * Read in an infoset in the form of XML text from a java.io.Reader
   *
   * @param reader the java.io.Reader to read the XML text from
   */
  @deprecated("This constructor is deprecated. Use XMLTextInfosetInputter(java.io.InputStream) instead.", "2.4.0")
  def this(reader: java.io.Reader) = this(new SXMLTextInfosetInputter(reader))

  /**
   * Read in an infoset in the form of XML text from a java.io.InputStream
   *
   * @param is the java.io.InputStream to read the XML text from
   */
  def this(is: java.io.InputStream) = this(new SXMLTextInfosetInputter(is))

  override val infosetInputter = inputter
}

/**
 * [[InfosetInputter]] to read an infoset represented as JSON from a java.io.InputStream
 */
class JsonInfosetInputter private (inputter: SJsonInfosetInputter)
  extends InfosetInputterProxy {

  /**
   * Read in an infoset in the form of json text from a java.io.Reader
   *
   * @param reader the java.io.Reader to read the json text from
   */
  @deprecated("This constructor is deprecated. Use JsonInfosetInputter(java.io.InputStream) instead.", "2.4.0")
  def this(reader: java.io.Reader) = this(new SJsonInfosetInputter(reader))

  /**
   * Read in an infoset in the form of json text from a java.io.InputStream
   *
   * @param is the java.io.InputStream to read the json text from
   */
  def this(is: java.io.InputStream) = this(new SJsonInfosetInputter(is))

  override val infosetInputter = inputter
}

/**
 * [[InfosetInputter]] to read an infoset represented as an org.jdom2.Document
 *
 * @param document the org.jdom2.Document infoset
 */
class JDOMInfosetInputter(document: org.jdom2.Document)
  extends InfosetInputterProxy {

  override val infosetInputter = new SJDOMInfosetInputter(document)
}

/**
 * [[InfosetInputter]] to read an infoset represented as an org.w3c.dom.Document
 *
 * @param document the org.w3c.dom.Document infoset. Note that w3c
 *                 Documents are not guaranteed to be thread-safe, even if all
 *                 users only read/traverse it. It is up to the user to ensure
 *                 that the Document passed into the W3CDOMInfosetInputter is
 *                 not read or written by other threads while the
 *                 W3CDOMInfosetInputter has access to it.
 */
class W3CDOMInfosetInputter(document: org.w3c.dom.Document)
  extends InfosetInputterProxy {

  override val infosetInputter = new SW3CDOMInfosetInputter(document)
}

/**
 * A proxy for InfosetInputters that are internal to Daffodil
 */
abstract class InfosetInputterProxy extends InfosetInputter {

  /**
   * The InfosetInputter to proxy infoset events to
   */
  protected val infosetInputter: SInfosetInputter

  override def getEventType() = infosetInputter.getEventType()
  override def getLocalName() = infosetInputter.getLocalName()
  override def getNamespaceURI() = infosetInputter.getNamespaceURI()
  override def getSimpleText(primType: NodeInfo.Kind) = infosetInputter.getSimpleText(primType: NodeInfo.Kind)
  override def hasNext() = infosetInputter.hasNext()
  override def isNilled() = infosetInputter.isNilled()
  override def next() = infosetInputter.next()
  override lazy val supportsNamespaces = infosetInputter.supportsNamespaces

  override def fini = infosetInputter.fini
}

/**
 * A proxy for InfosetOutputters that are internal to Daffodil
 */
abstract class InfosetOutputterProxy extends InfosetOutputter {

  /**
   * The InfosetOutputter to proxy infoset events to
   */
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
