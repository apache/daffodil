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


/**
 * Receive infoset events and forward them to one or more InfosetOutputters. A
 * thrown exception from any outputter is not caught and bubbles up resulting
 * in an SDE. No other outputters are called when an exception occurs.
 *
 * @param outputters
 *
 *    InfosetOutputters to send all events
 */
class TeeInfosetOutputter(outputters: InfosetOutputter*)
  extends InfosetOutputter {

  override def reset(): Unit = {
    outputters.foreach { _.reset() }
  }

  override def startSimple(simple: DISimple): Unit = {
    outputters.foreach { _.startSimple(simple) }
  }
  
  override def endSimple(simple: DISimple): Unit = {
    outputters.foreach { _.endSimple(simple) }
  }

  override def startComplex(complex: DIComplex): Unit = {
    outputters.foreach { _.startComplex(complex) }
  }

  override def endComplex(complex: DIComplex): Unit = {
    outputters.foreach { _.endComplex(complex) }
  }

  override def startArray(array: DIArray): Unit = {
    outputters.foreach { _.startArray(array) }
  }

  override def endArray(array: DIArray): Unit = {
    outputters.foreach { _.endArray(array) }
  }

  override def startDocument(): Unit = {
    outputters.foreach { _.startDocument() }
  }

  override def endDocument(): Unit = {
    outputters.foreach { _.endDocument() }
  }
}
