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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeBoolean
import org.apache.daffodil.runtime1.api.InfosetArray
import org.apache.daffodil.runtime1.api.InfosetComplexElement
import org.apache.daffodil.runtime1.api.InfosetSimpleElement
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType
import org.apache.daffodil.runtime1.infoset.{ InfosetInputter => SInfosetInputter }
import org.apache.daffodil.runtime1.infoset.{ InfosetOutputter => SInfosetOutputter }
import org.apache.daffodil.runtime1.infoset.{ JDOMInfosetInputter => SJDOMInfosetInputter }
import org.apache.daffodil.runtime1.infoset.{ JDOMInfosetOutputter => SJDOMInfosetOutputter }
import org.apache.daffodil.runtime1.infoset.{ JsonInfosetInputter => SJsonInfosetInputter }
import org.apache.daffodil.runtime1.infoset.{ JsonInfosetOutputter => SJsonInfosetOutputter }
import org.apache.daffodil.runtime1.infoset.{ NullInfosetOutputter => SNullInfosetOutputter }
import org.apache.daffodil.runtime1.infoset.{
  ScalaXMLInfosetInputter => SScalaXMLInfosetInputter
}
import org.apache.daffodil.runtime1.infoset.{
  ScalaXMLInfosetOutputter => SScalaXMLInfosetOutputter
}
import org.apache.daffodil.runtime1.infoset.{ W3CDOMInfosetInputter => SW3CDOMInfosetInputter }
import org.apache.daffodil.runtime1.infoset.{
  W3CDOMInfosetOutputter => SW3CDOMInfosetOutputter
}
import org.apache.daffodil.runtime1.infoset.{
  XMLTextInfosetInputter => SXMLTextInfosetInputter
}
import org.apache.daffodil.runtime1.infoset.{
  XMLTextInfosetOutputter => SXMLTextInfosetOutputter
}
import org.apache.daffodil.sapi.packageprivate._

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
  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String =
    getSimpleText(primType)

  /**
   * See getSimpleText(primType, runtimeProperties), which has a default
   * implementation to call this function without the runtimeProperties Map
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
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def startDocument(): Unit

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def endDocument(): Unit

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  @throws[Exception]
  def startSimple(diSimple: InfosetSimpleElement): Unit

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  @throws[Exception]
  def endSimple(diSimple: InfosetSimpleElement): Unit

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is started. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def startComplex(complex: InfosetComplexElement): Unit

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is ended. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def endComplex(complex: InfosetComplexElement): Unit

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is started. Various fields of
   *                DIArray can be accessed to determine things like the
   *                name, namespace, etc.
   */
  @throws[Exception]
  def startArray(array: InfosetArray): Unit

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is ended. Various fields of
   *                DIArray can be accessed to determine things like the
   *                name, namespace, etc.
   */
  @throws[Exception]
  def endArray(array: InfosetArray): Unit
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
class ScalaXMLInfosetOutputter() extends InfosetOutputterProxy {

  override val infosetOutputter = new SScalaXMLInfosetOutputter()

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
   * Output the infoset as XML Text, written to a java.io.OutputStream
   *
   * @param os the java.io.OutputStream to write the XML text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the XML.
   * @param xmlTextEscapeStyle determine whether to wrap values of elements of type
   *                       xs:string in CDATA tags in order to preserve
   *                       whitespace.
   */
  def this(
    os: java.io.OutputStream,
    pretty: Boolean,
    xmlTextEscapeStyle: XMLTextEscapeStyle.Value = XMLTextEscapeStyle.Standard
  ) = {
    this(
      new SXMLTextInfosetOutputter(
        os,
        pretty,
        XMLTextEscapeStyleConversions.styleToScala(xmlTextEscapeStyle)
      )
    )
  }

  override val infosetOutputter = outputter
}

/**
 * [[InfosetOutputter]] to build an infoset represented as JSON written to a java.io.OutputStream
 */
class JsonInfosetOutputter private (outputter: SJsonInfosetOutputter)
  extends InfosetOutputterProxy {

  /**
   * Output the infoset as json text, written to a java.io.OutputStream
   *
   * @param os the java.io.OutputStream to write the json text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the json.
   */
  def this(os: java.io.OutputStream, pretty: Boolean) =
    this(new SJsonInfosetOutputter(os, pretty))

  override val infosetOutputter = outputter
}

/**
 * [[InfosetOutputter]] to build an infoset represented as an org.jdom2.Document
 */
class JDOMInfosetOutputter() extends InfosetOutputterProxy {

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
class W3CDOMInfosetOutputter() extends InfosetOutputterProxy {

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
class NullInfosetOutputter() extends InfosetOutputterProxy {

  override val infosetOutputter = new SNullInfosetOutputter()
}

/**
 * [[InfosetInputter]] to read an infoset represented as a scala.xml.Node
 *
 * @param node the scala.xml.Node infoset
 */
class ScalaXMLInfosetInputter(node: scala.xml.Node) extends InfosetInputterProxy {

  override val infosetInputter = new SScalaXMLInfosetInputter(node)
}

/**
 * [[InfosetInputter]] to read an infoset represented as XML from a java.io.InputStream
 */
class XMLTextInfosetInputter private (inputter: SXMLTextInfosetInputter)
  extends InfosetInputterProxy {

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
class JDOMInfosetInputter(document: org.jdom2.Document) extends InfosetInputterProxy {

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
class W3CDOMInfosetInputter(document: org.w3c.dom.Document) extends InfosetInputterProxy {

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
  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {
    infosetInputter.getSimpleText(primType, runtimeProperties)
  }
  override def getSimpleText(primType: NodeInfo.Kind) = {
    // $COVERAGE-OFF$
    Assert.impossible()
    // $COVERAGE-ON$
  }
  override def hasNext() = infosetInputter.hasNext()
  override def isNilled() = infosetInputter.isNilled()
  override def next() = infosetInputter.next()
  override lazy val supportsNamespaces = infosetInputter.supportsNamespaces

  override def fini() = infosetInputter.fini()
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
  override def startDocument(): Unit = infosetOutputter.startDocument()
  override def endDocument(): Unit = infosetOutputter.endDocument()
  override def startSimple(diSimple: InfosetSimpleElement): Unit =
    infosetOutputter.startSimple(diSimple)
  override def endSimple(diSimple: InfosetSimpleElement): Unit =
    infosetOutputter.endSimple(diSimple)
  override def startComplex(complex: InfosetComplexElement): Unit =
    infosetOutputter.startComplex(complex)
  override def endComplex(complex: InfosetComplexElement): Unit =
    infosetOutputter.endComplex(complex)
  override def startArray(array: InfosetArray): Unit = infosetOutputter.startArray(array)
  override def endArray(array: InfosetArray): Unit = infosetOutputter.endArray(array)
}
