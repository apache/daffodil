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

import java.util.ArrayList;

import org.apache.daffodil.japi.infoset.InfosetOutputter;

// TODO: Shouldn't need to import things not in the japi package
import org.apache.daffodil.infoset.DIArray;
import org.apache.daffodil.infoset.DIComplex;
import org.apache.daffodil.infoset.DISimple;


public class TestInfosetOutputter extends InfosetOutputter {

    public ArrayList<TestInfosetEvent> events;

    TestInfosetOutputter() {
        events = new ArrayList<>();
    }

    @Override
    public void reset() {
        events = new ArrayList<>();
    }

    @Override
    public void startDocument() {
        events.add(TestInfosetEvent.startDocument());
    }

    @Override
    public void endDocument() {
        events.add(TestInfosetEvent.endDocument());
    }

    @Override
    public void startSimple(DISimple diSimple) {
        events.add(
            TestInfosetEvent.startSimple(
                diSimple.erd().name(),
                diSimple.erd().namedQName().namespace().toString(),
                diSimple.dataValueAsString(),
                diSimple.erd().isNillable() ? diSimple.isNilled() : null));
    }

    @Override
    public void endSimple(DISimple diSimple) {
        events.add(
            TestInfosetEvent.endSimple(
                diSimple.erd().name(),
                diSimple.erd().namedQName().namespace().toString()));
    }

    @Override
    public void startComplex(DIComplex diComplex) {
        events.add(
            TestInfosetEvent.startComplex(
                diComplex.erd().name(),
                diComplex.erd().namedQName().namespace().toString(),
                diComplex.erd().isNillable() ? diComplex.isNilled() : null));
    }

    @Override
    public void endComplex(DIComplex diComplex) {
        events.add(
            TestInfosetEvent.endComplex(
                diComplex.erd().name(),
                diComplex.erd().namedQName().namespace().toString()));
    }

    @Override
    public void startArray(DIArray diArray) {
    }

    @Override
    public void endArray(DIArray diArray) {
    }
}
