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

package org.apache.daffodil.processor.tdml;

import org.apache.daffodil.tdml.NoRoundTrip$;

import org.apache.daffodil.tdml.Runner;

import java.util.Arrays;
import java.net.URI;

import org.apache.daffodil.lib.util.Misc;
import org.junit.Test;
import org.xml.sax.InputSource;

import scala.Option;
import scala.jdk.javaapi.CollectionConverters;
import scala.xml.Elem;
import scala.xml.XML;
import scala.util.Right;

public class TestRunnerFactory {
  String testDir = "/org/apache/daffodil/tdml/";
  String testTdmlFile = "genericTdml.tdml";

  @Test
  public void testPrimaryConstructor() {
    URI tdmlUri = Misc.getRequiredResource("org/apache/daffodil/tdml/genericTdml.tdml");
    Right<scala.xml.Elem, String> rightURI = new Right<>(tdmlUri.toString());
    Runner runner = new Runner(
      rightURI,
      Option.apply(null),
      true,
      true,
      false,
      NoRoundTrip$.MODULE$,
      "off",
      CollectionConverters.asScala(Arrays.asList("daffodil", "ibm")).toSeq(),
      false,
      false);
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testOneArgConstructor() {
    String tdmlUri = Misc.getRequiredResource("org/apache/daffodil/tdml/genericTdml.tdml")
        .toString();
    InputSource tdmlResource = new InputSource(tdmlUri);
    Elem testXml = (Elem) XML.load(tdmlResource);
    Runner runner = new Runner(testXml);
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testTwoArgConstructor() {
    Runner runner = new Runner(testDir, testTdmlFile);
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testDirFileConstructor1() {
    Runner runner = new Runner("org/apache/daffodil/tdml", "genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testDirFileConstructor2() {
    Runner runner = new Runner("org/apache/daffodil/tdml/", "genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testDirFileConstructor3() {
    Runner runner = new Runner("/org/apache/daffodil/tdml", "genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testDirFileConstructor4() {
    Runner runner = new Runner("/org/apache/daffodil/tdml/", "genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testPathConstructor1() {
    Runner runner = new Runner("org/apache/daffodil/tdml/genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

  @Test
  public void testPathConstructor2() {
    Runner runner = new Runner("/org/apache/daffodil/tdml/genericTdml.tdml");
    runner.runOneTest("testPass");
    runner.reset();
  }

}
