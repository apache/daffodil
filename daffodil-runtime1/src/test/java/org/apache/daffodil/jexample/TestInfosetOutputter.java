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

import java.util.ArrayList;

import org.apache.daffodil.api.infoset.InfosetOutputter;
import org.apache.daffodil.runtime1.iapi.InfosetArray;
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement;
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement;

import static org.junit.Assert.assertEquals;

public class TestInfosetOutputter extends InfosetOutputter {


  public ArrayList<TestInfosetEvent> events = new ArrayList<>();

  TestInfosetOutputter() {
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
  public void startSimple(InfosetSimpleElement simple) {
    events.add(
        TestInfosetEvent.startSimple(
            simple.metadata().name(),
            simple.metadata().namespace(),
            simple.getText(),
            simple.metadata().isNillable() ? simple.isNilled() : null));
  }

  @Override
  public void endSimple(InfosetSimpleElement simple) {
    events.add(
        TestInfosetEvent.endSimple(
            simple.metadata().name(),
            simple.metadata().namespace()));
  }

  @Override
  public void startComplex(InfosetComplexElement complex) throws Exception {

    events.add(
        TestInfosetEvent.startComplex(
            complex.metadata().name(),
            complex.metadata().namespace(),
            complex.metadata().isNillable() ? complex.isNilled() : null));
  }

  @Override
  public void endComplex(InfosetComplexElement complex) {
    events.add(
        TestInfosetEvent.endComplex(
            complex.metadata().name(),
            complex.metadata().namespace()));
  }

  @Override
  public void startArray(InfosetArray array) {
  }

  @Override
  public void endArray(InfosetArray array) {
  }
}
