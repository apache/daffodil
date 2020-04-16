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

package org.apache.daffodil.infoset

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.equality._


trait XMLInfosetOutputter {

  def remapped(dataValueAsString: String) = XMLUtils.remapXMLIllegalCharactersToPUA(dataValueAsString)

  /**
   * String suitable for use in the text of a Processing Instruction.
   *
   * The text is a pseudo-XML string.
   */
  protected final def fmtInfo(diTerm: DITerm): Maybe[String] = {
    val pecXML = diTerm.parserEvalCache.toPseudoXML()
    val uecXML = diTerm.unparserEvalCache.toPseudoXML()
    val puxml = {
      (if (pecXML =:= "") "" else "\n" + pecXML) +
        (if (uecXML =:= "") "" else "\n" + uecXML)
    }
    Maybe(if (puxml =:= "") null else puxml)
  }

  final def addFmtInfo(diTerm: DITerm, elem: scala.xml.Elem, showFormatInfo: Boolean): scala.xml.Elem = {
    if (!showFormatInfo) return elem
    val maybeFI = fmtInfo(diTerm)
    val res =
      if (maybeFI.isEmpty) elem
      else {
        val fi = maybeFI.value
        val pi = new scala.xml.ProcInstr("formatInfo", fi)
        val res = elem.copy(child = elem.child :+ pi)
        res
      }
    res
  }

}
