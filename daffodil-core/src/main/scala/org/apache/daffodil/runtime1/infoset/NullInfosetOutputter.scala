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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.api
import org.apache.daffodil.api.infoset.InfosetArray
import org.apache.daffodil.api.infoset.InfosetComplexElement
import org.apache.daffodil.api.infoset.InfosetSimpleElement

/**
 * Ignores all infoset events, outputting nothing
 */
class NullInfosetOutputter() extends api.infoset.InfosetOutputter {

  override def reset(): Unit = {}

  override def startSimple(simple: InfosetSimpleElement): Unit = {}
  override def endSimple(simple: InfosetSimpleElement): Unit = {}

  override def startComplex(complex: InfosetComplexElement): Unit = {}
  override def endComplex(complex: InfosetComplexElement): Unit = {}

  override def startArray(array: InfosetArray): Unit = {}
  override def endArray(array: InfosetArray): Unit = {}

  override def startDocument(): Unit = {}
  override def endDocument(): Unit = {}
}
