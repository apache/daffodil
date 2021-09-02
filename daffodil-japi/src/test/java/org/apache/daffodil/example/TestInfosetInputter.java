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

package org.apache.daffodil.example;

import org.apache.daffodil.japi.infoset.InfosetInputter;

// TODO: Shouldn't need to import things not in the japi package
import org.apache.daffodil.dpath.NodeInfo;
import org.apache.daffodil.infoset.InfosetInputterEventType;


public class TestInfosetInputter extends InfosetInputter {

    private TestInfosetEvent[] events;
    private int curEventIndex;

    TestInfosetInputter(TestInfosetEvent... _events) {
        events = _events;
        curEventIndex = 0;
    }

    @Override
    public InfosetInputterEventType getEventType() {
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

    // TODO: This should return a MaybeBoolean, but that is Scala value class
    // with underlying type being an int, so this must return an int. Returning
    // 1 means true, 0 is false, and -1 is not set
    @Override
    public int isNilled() {
        Boolean isNilled = events[curEventIndex].isNilled;
        if (isNilled == null) { return -1; }
        else return isNilled ? 1 : 0;
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
    public boolean supportsNamespaces() {
        return true;
    }

    @Override
    public void fini() {
        // noop
    }
}
