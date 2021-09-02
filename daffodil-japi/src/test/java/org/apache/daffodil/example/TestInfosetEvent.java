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

// TODO: Shouldn't need to import things not in the japi package
import org.apache.daffodil.dpath.NodeInfo;
import org.apache.daffodil.infoset.InfosetInputterEventType;
import org.apache.daffodil.infoset.InfosetInputterEventType.EndDocument$;
import org.apache.daffodil.infoset.InfosetInputterEventType.EndElement$;
import org.apache.daffodil.infoset.InfosetInputterEventType.StartDocument$;
import org.apache.daffodil.infoset.InfosetInputterEventType.StartElement$;


public class TestInfosetEvent {

    InfosetInputterEventType eventType;
    String localName;
    String namespaceURI;
    String simpleText;
    // null means no null state specified ( which is required for
    // non-nullable elements). Boolean.TRUE or Boolean.FALSE means it is
    // nullable and has the given value
    Boolean isNilled;

    public TestInfosetEvent(InfosetInputterEventType _eventType, String _localName, String _namespaceURI, String _simpleText, Boolean _isNilled) {
        this.eventType = _eventType;
        this.localName = _localName;
        this.namespaceURI = _namespaceURI;
        this.simpleText = _simpleText;
        this.isNilled = _isNilled;
    }

    public boolean equals(Object o) {
        if (!(o instanceof TestInfosetEvent)) {
            return false;
        }
        TestInfosetEvent that = (TestInfosetEvent)o;
        return
            this.eventType == that.eventType &&
            java.util.Objects.equals(this.localName, that.localName) &&
            java.util.Objects.equals(this.namespaceURI, that.namespaceURI) &&
            java.util.Objects.equals(this.simpleText, that.simpleText) &&
            java.util.Objects.equals(this.isNilled, that.isNilled);

    }

    static TestInfosetEvent startDocument() {
        return new TestInfosetEvent (StartDocument$.MODULE$, null, null, null, null);
    }

    static TestInfosetEvent startComplex(String name, String namespace) {
        return startComplex(name, namespace, null);
    }

    static TestInfosetEvent startComplex(String name, String namespace, Boolean isNilled) {
        return new TestInfosetEvent (StartElement$.MODULE$, name, namespace, null, isNilled);
    }

    static TestInfosetEvent startSimple(String name, String namespace, String text) {
        return startSimple(name, namespace, text, null);
    }

    static TestInfosetEvent startSimple(String name, String namespace, String text, Boolean isNilled) {
        return new TestInfosetEvent (StartElement$.MODULE$, name, namespace, text, isNilled);
    }

    static TestInfosetEvent endComplex(String name, String namespace) {
        return new TestInfosetEvent (EndElement$.MODULE$, name, namespace, null, null);
    }

    static TestInfosetEvent endSimple(String name, String namespace) {
        return new TestInfosetEvent (EndElement$.MODULE$, name, namespace, null, null);
    }

    static TestInfosetEvent endDocument() {
        return new TestInfosetEvent (EndDocument$.MODULE$, null, null, null, null);
    }
}
