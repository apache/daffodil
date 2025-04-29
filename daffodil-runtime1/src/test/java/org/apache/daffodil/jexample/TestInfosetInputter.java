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

package org.apache.daffodil.jexample;

import org.apache.daffodil.api.infoset.Infoset;
import org.apache.daffodil.api.infoset.InfosetInputter;
import org.apache.daffodil.runtime1.dpath.NodeInfo;
import scala.Option;


public class TestInfosetInputter extends InfosetInputter {

  private TestInfosetEvent[] events;
  private int curEventIndex;

  TestInfosetInputter(TestInfosetEvent... _events) {
    events = _events;
    curEventIndex = 0;
  }

  @Override
  public Infoset.InfosetInputterEventType getEventType() {
    return events[curEventIndex].eventType;
  }

  @Override
  public String getLocalName() {
    return events[curEventIndex].localName;
  }

  @Override
  public String getNamespaceURI() {
    return events[curEventIndex].namespaceURI;
  }

  @Override
  public String getSimpleText(NodeInfo.Kind primType) {
    return events[curEventIndex].simpleText;
  }

  @Override
  public Option<Boolean> isNilled() {
    Boolean isNilled = events[curEventIndex].isNilled;
    if (isNilled == null) {
      return Option.apply(null);
    } else return Option.apply(isNilled);
  }

  @Override
  public boolean hasNext() {
    return curEventIndex + 1 < this.events.length;
  }

  @Override
  public void next() {
    curEventIndex++;
  }

  @Override
  public boolean getSupportsNamespaces() {
    return true;
  }

  @Override
  public void fini() {
    // noop
  }
}
