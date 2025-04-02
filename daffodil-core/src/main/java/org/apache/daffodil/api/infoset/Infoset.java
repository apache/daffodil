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

package org.apache.daffodil.api.infoset;

import org.apache.daffodil.api.InputSourceDataInputStream;
import org.apache.daffodil.runtime1.infoset.JsonInfosetOutputter;
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetOutputter;
import scala.xml.Node;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;

/**
 * Contains factory methods for InfosetInputters and Outputters
 */
public class Infoset {
  /**
   * private constructor
   */
  private Infoset() {
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as a scala.xml.Node
   *
   * @return InfosetOutputter
   */
  public static ScalaXMLInfosetOutputter getScalaXMLInfosetOutputter() {
    return new org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter(false);
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as XML written to a java.io.OutputStream
   * <p>
   * Output the infoset as XML Text, written to a java.io.OutputStream
   *
   * @param os     the java.io.OutputStream to write the XML text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the XML.
   * @return InfosetOutputter
   */
  public static InfosetOutputter getXMLTextInfosetOutputter(OutputStream os, boolean pretty) {
    return new XMLTextInfosetOutputter(os, pretty, XMLTextEscapeStyle.Standard, false);
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as XML written to a java.io.OutputStream
   * <p>
   * Output the infoset as XML Text, written to a java.io.OutputStream
   *
   * @param os                 the java.io.OutputStream to write the XML text to
   * @param pretty             enable or disable pretty printing. Pretty printing will only
   *                           insert indentation and newlines where it will not affect the
   *                           content of the XML.
   * @param xmlTextEscapeStyle determine whether to wrap values of elements of type
   *                           xs:string in CDATA tags in order to preserve
   *                           whitespace.
   * @return InfosetOutputter
   */
  public static InfosetOutputter getXMLTextInfosetOutputter(OutputStream os, boolean pretty, XMLTextEscapeStyle xmlTextEscapeStyle) {
    return new XMLTextInfosetOutputter(os, pretty, xmlTextEscapeStyle, false);
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as JSON written to a java.io.OutputStream
   * Output the infoset as json text, written to a java.io.OutputStream
   *
   * @param os     the java.io.OutputStream to write the json text to
   * @param pretty enable or disable pretty printing. Pretty printing will only
   *               insert indentation and newlines where it will not affect the
   *               content of the json.
   * @return InfosetOutputter
   */
  public static InfosetOutputter getJsonInfosetOutputter(OutputStream os, boolean pretty) {
    return new JsonInfosetOutputter(os, pretty);
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as an org.jdom2.Document
   *
   * @return InfosetOutputter
   */
  public static JDOMInfosetOutputter getJDOMInfosetOutputter() {
    return new org.apache.daffodil.runtime1.infoset.JDOMInfosetOutputter();
  }

  /**
   * {@link InfosetOutputter} to build an infoset represented as an org.w3c.dom.Document
   *
   * @return InfosetOutputter
   */
  public static W3CDOMInfosetOutputter getW3CDOMInfosetOutputter() {
    return new org.apache.daffodil.runtime1.infoset.W3CDOMInfosetOutputter();
  }

  /**
   * {@link InfosetOutputter} that does not build an infoset representation, ignoring
   * all {@link InfosetOutputter} events
   *
   * @return InfosetOutputter
   * \
   */
  public static InfosetOutputter getNullInfosetOutputter() {
    return new org.apache.daffodil.runtime1.infoset.NullInfosetOutputter();
  }

  /**
   * {@link InfosetInputter} to read an infoset represented as a scala.xml.Node
   *
   * @param node the scala.xml.Node infoset
   * @return InfosetInputter
   */
  public static InfosetInputter getScalaXMLInfosetInputter(Node node) {
    return new org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter(node);
  }

  /**
   * {@link InfosetInputter} to read an infoset represented as XML from a java.io.InputStream
   * <p>
   * Read in an infoset in the form of XML text from a java.io.InputStream
   *
   * @param is the java.io.InputStream to read the XML text from
   * @return InfosetInputter
   */
  public static InfosetInputter getXMLTextInfosetInputter(InputStream is) {
    return new org.apache.daffodil.runtime1.infoset.XMLTextInfosetInputter(is);
  }

  /**
   * {@link InfosetInputter} to read an infoset represented as JSON from a java.io.InputStream
   * <p>
   * Read in an infoset in the form of json text from a java.io.InputStream
   *
   * @param is the java.io.InputStream to read the json text from
   * @return InfosetInputter
   */
  public static InfosetInputter getJsonInfosetInputter(InputStream is) {
    return new org.apache.daffodil.runtime1.infoset.JsonInfosetInputter(is);
  }

  /**
   * {@link InfosetInputter} to read an infoset represented as an org.jdom2.Document
   *
   * @param document the org.jdom2.Document infoset
   * @return InfosetInputter
   */
  public static InfosetInputter getJDOMInfosetInputter(org.jdom2.Document document) {
    return new org.apache.daffodil.runtime1.infoset.JDOMInfosetInputter(document);
  }

  /**
   * {@link InfosetInputter} to read an infoset represented as an org.w3c.dom.Document
   *
   * @param document the org.w3c.dom.Document infoset. Note that w3c
   *                 Documents are not guaranteed to be thread-safe, even if all
   *                 users only read/traverse it. It is up to the user to ensure
   *                 that the Document passed into the W3CDOMInfosetInputter is
   *                 not read or written by other threads while the
   *                 W3CDOMInfosetInputter has access to it.
   * @return InfosetInputter
   */
  public static InfosetInputter getW3CDOMInfosetInputter(org.w3c.dom.Document document) {
    return new org.apache.daffodil.runtime1.infoset.W3CDOMInfosetInputter(document);
  }

  /**
   * Create an InputSourceDataInputStream from a java.io.InputStream
   *
   * @param is input stream to create from
   * @return InputSourceDataInputStream from a java.io.InputStream
   */
  public static InputSourceDataInputStream getInputSourceDataInputStream(InputStream is) {
    return org.apache.daffodil.io.InputSourceDataInputStream.apply(is);
  }

  /**
   * Create an InputSourceDataInputStream from a java.nio.ByteBuffer
   *
   * @param bb byte buffer to create from
   * @return InputSourceDataInputStream from a java.nio.ByteBuffer
   */
  public static InputSourceDataInputStream getInputSourceDataInputStream(ByteBuffer bb) {
    return org.apache.daffodil.io.InputSourceDataInputStream.apply(bb);
  }

  /**
   * Create an InputSourceDataInputStream from a byte array
   *
   * @param arr byte array to create from
   * @return InputSourceDataInputStream from a byte array
   */
  public static InputSourceDataInputStream getInputSourceDataInputStream(byte[] arr) {
    return org.apache.daffodil.io.InputSourceDataInputStream.apply(arr);
  }

  /**
   * These are the events that a derived specific InfosetInputter
   * creates.
   * <p>
   * The InfosetInputter base class figures out Daffodil InfosetEvents from
   * the call-backs providing these derived-class events types.
   */
  public enum InfosetInputterEventType {
    StartDocument,
    EndDocument,
    StartElement,
    EndElement;
  }
}
