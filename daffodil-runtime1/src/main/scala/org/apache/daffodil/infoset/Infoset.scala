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

import org.apache.daffodil.Implicits.ImplicitsSuppressUnusedImportWarning
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.xml.NS

object INoWarn2 { ImplicitsSuppressUnusedImportWarning() }

trait InfosetArray {
  def append(ie: InfosetElement): Unit
  def getOccurrence(occursIndex: Long): InfosetElement
  def length: Long

}

trait InfosetElement extends InfosetItem {

  def parent: InfosetComplexElement
  def setParent(p: InfosetComplexElement): Unit

  def array: Maybe[InfosetArray]
  def setArray(a: InfosetArray): Unit

  def isNilled: Boolean
  def setNilled(): Unit

  def isEmpty: Boolean

  def valid: MaybeBoolean
  def setValid(validity: Boolean): Unit

  /**
   * Retrieve the schema component that gave rise to this infoset
   * item.
   */
  def runtimeData: ElementRuntimeData
  def namespace: NS
  def name: String
  def isHidden: Boolean
  def setHidden(): Unit

}

trait InfosetComplexElement extends InfosetElement {

  def getChild(erd: ElementRuntimeData, tunable: DaffodilTunables): InfosetElement
  def getChildArray(erd: ElementRuntimeData, tunable: DaffodilTunables): InfosetArray

  /**
   * Determines slotInParent from the ERD of the infoset element arg.
   * Hooks up the parent pointer of the new child to reference this.
   *
   * When slot contains an array, this appends to the end of the array.
   */
  def addChild(e: InfosetElement, tunable: DaffodilTunables): Unit

}

trait InfosetSimpleElement extends InfosetElement {

  def dataValue: DataValuePrimitiveNullable

  /**
   * Caches the string so we're not allocating strings just to do facet checks
   */
  def dataValueAsString: String
  def setDataValue(s: DataValuePrimitiveNullable): Unit
  def isDefaulted: Boolean
}

trait InfosetDocument extends InfosetItem {
}

trait InfosetItem {
  /**
   * The totalElementCount is the total count of how many elements this InfosetItem contains.
   *
   * (Used to call this 'size', but size is often a length-like thing, so changed name
   * to be more distinctive)
   */
  def totalElementCount: Long
}
