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
 * Receive infoset events and forward them to one or more InfosetOutputters.
 * For infoset events that return a boolean, this returns true only if all
 * outputters return true, otherwise false is returned. Additionally, all
 * events are called on all the outputters regardless of the return of any
 * previous outputters.
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

  override def startSimple(simple: DISimple): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.startSimple(simple)
    }
  }
  
  override def endSimple(simple: DISimple): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.endSimple(simple)
    }
  }

  override def startComplex(complex: DIComplex): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.startComplex(complex)
    }
  }

  override def endComplex(complex: DIComplex): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.endComplex(complex)
    }
  }

  override def startArray(array: DIArray): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.startArray(array)
    }
  }

  override def endArray(array: DIArray): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.endArray(array)
    }
  }

  override def startDocument(): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.startDocument()
    }
  }

  override def endDocument(): Boolean = {
    outputters.foldLeft(true) { case (res, outputter) =>
      res & outputter.endDocument()
    }
  }
}
