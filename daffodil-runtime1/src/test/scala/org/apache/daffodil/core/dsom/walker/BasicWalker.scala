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

package org.apache.daffodil.core.dsom.walker

import scala.collection.mutable.ArrayBuffer

class BasicWalker(
  ignoreTypeWrappers: Boolean = false,
  onlyElements: Boolean = false,
  ignoreEndEvents: Boolean = true
) extends AbstractDSOMWalker {

  var nodeArr: ArrayBuffer[AnyRef] = ArrayBuffer()

  private def addViewElement(viewElement: AnyRef): Unit = {
    viewElement match {
      case _: ElementBaseView => nodeArr += viewElement
      case _: TermView => if (!onlyElements) nodeArr += viewElement
      case _ => if (!ignoreTypeWrappers) nodeArr += viewElement
    }
  }

  override protected def onWalkBegin(root: RootView): Unit = addViewElement(root)

  override protected def onWalkEnd(root: RootView): Unit =
    if (!ignoreEndEvents) addViewElement(root)

  override def onTermBegin(termElement: TermView): Unit = addViewElement(termElement)

  override def onTermEnd(termElement: TermView): Unit =
    if (!ignoreEndEvents) addViewElement(termElement)

  override def onTypeBegin(typeElement: TypeView): Unit = addViewElement(typeElement)

  override def onTypeEnd(typeElement: TypeView): Unit =
    if (!ignoreEndEvents) addViewElement(typeElement)
}
