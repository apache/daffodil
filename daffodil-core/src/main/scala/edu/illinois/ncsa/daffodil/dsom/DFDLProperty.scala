/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node

final class DFDLProperty(xmlArg: Node, formatAnnotation: DFDLFormatAnnotation)
  extends DFDLAnnotation(xmlArg, formatAnnotation.annotatedSC)
  with NamedMixin {

  override lazy val path = formatAnnotation.path + "::" + prettyName

  override lazy val schemaComponent: LookupLocation = formatAnnotation.annotatedSC

  override lazy val schemaDocument = formatAnnotation.schemaDocument
  override lazy val uriString = xmlSchemaDocument.uriString

  // TODO: if we grab the value from here, then any qnames inside that value
  // have to be resolved by THIS Object
  lazy val value = xml match {
    case <dfdl:property/> => ""
    case <dfdl:property>{ valueNodes @ _* }</dfdl:property> => {
      //
      // We have to implement our own trim logic.
      // and that is somewhat subtle. E.g., textNumberPattern where
      // spaces are meaningful active characters. lengthPattern,
      // assert patterns, etc.
      //
      // Inside dfdl:property, since it is an element, XML's typical
      // whitespace fungibility applies. So use CDATA if you care
      // about space inside these.
      //
      val values = valueNodes.flatMap { valueNode =>
        valueNode match {
          case scala.xml.PCData(s) => Some(valueNode)
          case scala.xml.Text(s) => {
            if (s.matches("""\s+""")) {
              // all whitespace. Remove the node.
              None
            } else {
              val trimmed = s.trim
              if (trimmed.length == 0) None
              else Some(scala.xml.Text(trimmed))
            }
          }
          case scala.xml.Comment(_) => None
          case scala.xml.EntityRef(_) => Some(valueNode)
          case _: scala.xml.Atom[_] => Some(valueNode) // &lt; comes through as this... should be EntityRef
        }
      }
      val res = values.map { _.text }.mkString
      res
    }
  }

  override lazy val name = getAttributeRequired("name")

}
