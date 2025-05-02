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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.daffodil.api.Daffodil;
import org.apache.daffodil.api.DaffodilParseXMLReader;
import org.apache.daffodil.api.DaffodilUnparseContentHandler;
import org.apache.daffodil.api.DaffodilXMLEntityResolver;
import org.apache.daffodil.api.DataProcessor;
import org.apache.daffodil.api.Diagnostic;
import org.apache.daffodil.api.LocationInSchemaFile;
import org.apache.daffodil.api.ParseResult;
import org.apache.daffodil.api.validation.ValidatorsFactory;
import org.apache.daffodil.japi.SAXErrorHandlerForAPITest;
import org.apache.daffodil.api.UnparseResult;
import org.apache.daffodil.api.compiler.ProcessorFactory;
import org.apache.daffodil.api.exceptions.DaffodilUnhandledSAXException;
import org.apache.daffodil.api.exceptions.DaffodilUnparseErrorSAXException;
import org.apache.daffodil.api.exceptions.ExternalVariableException;
import org.apache.daffodil.api.exceptions.InvalidParserException;
import org.apache.daffodil.api.exceptions.InvalidUsageException;
import org.apache.daffodil.api.infoset.Infoset;
import org.apache.daffodil.api.infoset.InfosetInputter;
import org.apache.daffodil.api.infoset.InfosetOutputter;
import org.apache.daffodil.api.infoset.JDOMInfosetOutputter;
import org.apache.daffodil.api.infoset.XMLTextEscapeStyle;
import org.apache.daffodil.api.InputSourceDataInputStream;

import org.apache.commons.io.FileUtils;
import org.jdom2.output.Format;
import org.junit.Test;

import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;

import javax.xml.XMLConstants;

import java.nio.charset.StandardCharsets;


public class TestAPI {

  /**
   * Best practices for XML loading are to turn off anything that could lead to
   * insecurity.
   * <p>
   * This is probably unnecessary in the case of these tests, but as these tests
   * are also used to illustrate API usage, this exemplifies best practice.
   */
  public static void setSecureDefaults(XMLReader xmlReader)
      throws SAXNotSupportedException, SAXNotRecognizedException {
    xmlReader.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
    // since we're not really sure what they mean by secure processing
    // we make doubly sure by setting these ourselves also.
    xmlReader.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    xmlReader.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
    xmlReader.setFeature("http://xml.org/sax/features/external-general-entities", false);
  }

  String SAX_NAMESPACES_FEATURE = "http://xml.org/sax/features/namespaces";
  String SAX_NAMESPACE_PREFIXES_FEATURE = "http://xml.org/sax/features/namespace-prefixes";

  private java.io.File getResource(String resPath) {
    try {
      return new java.io.File(this.getClass().getResource(resPath).toURI());
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * This is a test-only helper function used to serialize and deserialize a
   * DataProcessor to ensure all JAPI classes that need to extend
   * Serializable do so appropriately.
   * <p>
   * All of the JAPI tests create a DataProcessor. To test that we correctly
   * made all the necessary changes to make the JAPI DataProcessor
   * serializable, it is important to serialize and deserialize that
   * DataProcessor before use in the tests. This function acts as a helper
   * function to accomplish that task.
   * <p>
   * So this functions accepts a JAPI DataProcessor, serializes and deserializes
   * that DataProcessor in memory, and then returns the result.
   */
  private DataProcessor reserializeDataProcessor(DataProcessor dp) throws IOException, ClassNotFoundException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    dp.save(Channels.newChannel(baos));
    baos.close();

    DataProcessor result = null;
    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
    try {
      result = Daffodil.compiler().reload(Channels.newChannel(bais));
    } catch (InvalidParserException e) {
      fail("Unable to reload data processor");
    }
    return result;
  }

  @Test
  public void testJavaAPI1() throws IOException, ClassNotFoundException {
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertTrue(debugger.lines.size() > 0);
      assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
      assertTrue(debugger.getCommand().equals("trace"));

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("42", bos.toString());
    }
  }

  // This is a duplicate of test testJavaAPI1 that serializes the parser
  // before executing the test.
  @Test
  public void testJavaAPI1_A() throws Exception {
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");

    // Serialize the parser to memory, then deserialize for parsing.
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    WritableByteChannel output = Channels.newChannel(os);
    dp.save(output);

    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
    ReadableByteChannel input = Channels.newChannel(is);
    org.apache.daffodil.api.Compiler compiler = Daffodil.compiler();
    DataProcessor parser = compiler.reload(input);
    parser = parser.withDebuggerRunner(debugger);
    parser = parser.withDebugging(true);

    File data = getResource("/test/api/myData.dat");
    // This test uses a byte array here, just so as to be sure to exercise
    // the constructor for creating an InputSourceDataInputStream from a byte array
    // and byte buffer.
    byte[] ba = FileUtils.readFileToByteArray(data);
    ByteBuffer bb = ByteBuffer.wrap(ba);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(bb)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = parser.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertTrue(debugger.lines.size() > 0);
      assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
      assertTrue(debugger.getCommand().equals("trace"));

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("42", bos.toString());
    }
  }

  // This is a duplicate of test testJavaAPI1 that serializes the parser
  // before executing the test.
  @Test
  public void testJavaAPI1_A_FullFails() throws Exception {
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);

    // Serialize the parser to memory, then deserialize for parsing.
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    WritableByteChannel output = Channels.newChannel(os);
    dp.save(output);

    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
    ReadableByteChannel input = Channels.newChannel(is);
    org.apache.daffodil.api.Compiler compiler = Daffodil.compiler();
    DataProcessor parser = compiler.reload(input);

    try {
      parser = parser.withValidator(ValidatorsFactory.getXercesValidator(parser.getMainSchemaURIForFullValidation()));
      fail();
    } catch (InvalidUsageException e) {
      assertEquals("'Full' validation not allowed when using a restored parser.", e.getMessage());
    }
  }

  @Test
  public void testJavaAPI2() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myDataBroken.dat");
    // This test uses a byte array here, just so as to be sure to exercise
    // the constructor for creating an InputSourceDataInputStream from a byte array
    // and byte buffer.
    byte[] ba = FileUtils.readFileToByteArray(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(ba)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);

      // TODO: NEED a java friendly way to get the status of the outputter.
      // Scala enums don't work well
      // assertTrue(outputter.getStatus() != Status.DONE); // This is a hack
      // so that Java doesn't have to know about Nope/Maybe, need to figure
      // out better api that is Java compatible
      assertTrue(res.isError());
      java.util.List<Diagnostic> diags = res.getDiagnostics();
      assertEquals(1, diags.size());
      Diagnostic d = diags.get(0);
      // System.err.println(d.getMessage());
      assertTrue(d.getMessage().contains("int"));
      assertTrue(d.getMessage().contains("Not an int"));
      assertTrue(d.getDataLocations().toString().contains("10"));
      java.util.List<LocationInSchemaFile> locs = d.getLocationsInSchemaFiles();
      assertEquals(1, locs.size());
      LocationInSchemaFile loc = locs.get(0);
      assertTrue(loc.asString().contains("mySchema1.dfdl.xsd"));
      // above is mySchema1 as it reports the element ref location,
      // not element decl.
    }
  }

  /**
   * Verify that we can detect when the parse did not consume all the data.
   *
   * @throws IOException
   */
  @Test
  public void testJavaAPI3() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    pf = pf.withDistinguishedRootNode("e3", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData16.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertEquals(2, res.location().bytePos1b());
      assertEquals(9, res.location().bitPos1b());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("9", bos.toString());
    }
  }

  // This is a duplicate of test testJavaAPI3 that serializes the parser
  // before executing the test.
  @Test
  public void testJavaAPI3_A() throws Exception {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    pf = pf.withDistinguishedRootNode("e3", null);
    DataProcessor dp = pf.onPath("/");

    // Serialize the parser to memory, then deserialize for parsing.
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    WritableByteChannel output = Channels.newChannel(os);
    dp.save(output);

    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
    ReadableByteChannel input = Channels.newChannel(is);
    org.apache.daffodil.api.Compiler compiler = Daffodil.compiler();
    DataProcessor parser = compiler.reload(input);

    java.io.File file = getResource("/test/api/myData16.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = parser.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertEquals(2, res.location().bytePos1b());
      assertEquals(9, res.location().bitPos1b());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("9", bos.toString());
    }
  }

  @Test
  public void testJavaAPI4b() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    File schemaFileName = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFileName, "e4", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData2.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertEquals(5, res.location().bytePos1b());
      assertEquals(33, res.location().bitPos1b());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("data", bos.toString());
    }
  }

  @Test
  public void testJavaAPI5() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    File schemaFileName = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFileName, "e4", null); // e4 is a 4-byte long string
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData3.dat"); // contains 5
    // bytes
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertEquals(5, res.location().bytePos1b());
      assertEquals(33, res.location().bitPos1b());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("data", bos.toString());
    }
  }

  /***
   * Verify that the compiler throws a FileNotFound exception when fed a list
   * of schema files that do not exist.
   *
   * @throws IOException
   */
  @Test
  public void testJavaAPI6() throws IOException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = new java.io.File("/test/api/notHere1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    assertTrue(pf.isError());
    List<Diagnostic> diags = pf.getDiagnostics();
    boolean found1 = false;
    for (Diagnostic d : diags) {
      if (d.getMessage().contains("notHere1")) {
        found1 = true;
      }
    }
    assertTrue(found1);
  }

  /**
   * Tests a user submitted case where the XML appears to be serializing odd
   * xml entities into the output.
   *
   * @throws IOException
   */
  @Test
  public void testJavaAPI7() throws IOException, ClassNotFoundException {
    // TODO: This is due to the fact that we are doing several conversions
    // back and forth between Scala.xml.Node and JDOM. And the conversions
    // both use XMLOutputter to format the result (which escapes the
    // entities).
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/TopLevel.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/01very_simple.txt");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"));
    }
  }

  /**
   * This test is nearly identical to testJavaAPI7. The only difference is
   * that this test uses double newline as a terminator for the first element
   * in the sequence rather than double newline as a separator for the
   * sequence
   *
   * @throws IOException
   */
  @Test
  public void testJavaAPI8() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/TopLevel.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel2", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/01very_simple.txt");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"));
    }
  }

  /**
   * Verify that calling result() on the ParseResult multiple times does not
   * error.
   */
  @Test
  public void testJavaAPI9() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/TopLevel.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel2", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/01very_simple.txt");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);

      org.jdom2.Document doc1 = outputter.getResult();

      java.io.ByteArrayOutputStream bos1 = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc1 = java.nio.channels.Channels.newChannel(bos1);
      InfosetInputter inputter1 = Infoset.getJDOMInfosetInputter(doc1);
      UnparseResult res2 = dp.unparse(inputter1, wbc1);
      err = res2.isError();
      assertFalse(err);
      assertTrue(bos1.toString().contains("Return-Path: <bob@smith.com>"));

      org.jdom2.Document doc2 = outputter.getResult();

      java.io.ByteArrayOutputStream bos2 = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc2 = java.nio.channels.Channels.newChannel(bos2);
      InfosetInputter inputter2 = Infoset.getJDOMInfosetInputter(doc2);
      UnparseResult res3 = dp.unparse(inputter2, wbc2);
      err = res3.isError();
      assertFalse(err);
      assertTrue(bos2.toString().contains("Return-Path: <bob@smith.com>"));
    }
  }

  /**
   * Verify that hidden elements do not appear in the resulting infoset
   */
  @Test
  public void testJavaAPI10() throws IOException, ClassNotFoundException {

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema4.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData4.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      org.jdom2.Document doc = outputter.getResult();
      org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
      xo.setFormat(Format.getPrettyFormat());
      org.jdom2.Element rootNode = doc.getRootElement();
      org.jdom2.Element hidden = rootNode.getChild("hiddenElement", rootNode.getNamespace());
      assertTrue(null == hidden);
    }
  }

  /**
   * Verify that nested elements do not appear as duplicates
   */
  @Test
  public void testJavaAPI11() throws IOException, ClassNotFoundException {

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema5.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData5.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      org.jdom2.Document doc = outputter.getResult();
      org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
      xo.setFormat(Format.getPrettyFormat());
      // xo.output(doc, System.out);
      org.jdom2.Element rootNode = doc.getRootElement();
      org.jdom2.Element elementGroup = rootNode.getChild("elementGroup", null); // local
      // element
      // names
      // are
      // unqualified
      assertTrue(null != elementGroup);
      org.jdom2.Element groupE2 = elementGroup.getChild("e2", null);
      assertTrue(null != groupE2);
      org.jdom2.Element groupE3 = elementGroup.getChild("e3", null);
      assertTrue(null != groupE3);
      org.jdom2.Element rootE2 = rootNode.getChild("e2", null);
      assertTrue(null == rootE2);
      org.jdom2.Element rootE3 = rootNode.getChild("e3", null);
      assertTrue(null == rootE3);
    }
  }

  @Test
  public void testJavaAPI12() throws IOException, ClassNotFoundException {
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();


    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);

      assertTrue(debugger.lines.size() > 0);
      assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
    }
  }

  @Test
  public void testJavaAPI13() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via Compiler.
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File extVarsFile = getResource("/test/api/external_vars_1.xml");
    java.io.File schemaFile = getResource("/test/api/mySchemaWithVars.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);

    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);
    dp = dp.withExternalVariables(extVarsFile);

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      org.jdom2.Document doc = outputter.getResult();
      org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
      xo.setFormat(Format.getPrettyFormat());
      String docString = xo.outputString(doc);
      boolean containsVar1 = docString.contains("var1Value");
      boolean containsVar1Value = docString.contains("externallySet");
      assertTrue(containsVar1);
      assertTrue(containsVar1Value);
    }
  }

  @Test
  public void testJavaAPI14() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via DataProcessor.
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File extVarFile = getResource("/test/api/external_vars_1.xml");
    java.io.File schemaFile = getResource("/test/api/mySchemaWithVars.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);
    dp = dp.withExternalVariables(extVarFile);

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      org.jdom2.Document doc = outputter.getResult();
      org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
      xo.setFormat(Format.getPrettyFormat());
      String docString = xo.outputString(doc);
      boolean containsVar1 = docString.contains("var1Value");
      boolean containsVar1Value = docString.contains("externallySet");
      assertTrue(containsVar1);
      assertTrue(containsVar1Value);

      assertTrue(debugger.lines.size() > 0);
      assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
    }
  }

  @Test
  public void testJavaAPI15() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myInfosetBroken.xml");
    org.jdom2.input.SAXBuilder builder = new org.jdom2.input.SAXBuilder();

    org.jdom2.Document doc = null;
    try {
      doc = builder.build(file);
    } catch (Exception e) {
      fail();
    }

    java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
    java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
    InfosetInputter inputter = Infoset.getJDOMInfosetInputter(doc);
    UnparseResult res = dp.unparse(inputter, wbc);
    boolean err = res.isError();
    assertTrue(err);

    java.util.List<Diagnostic> diags = res.getDiagnostics();
    assertEquals(1, diags.size());
    Diagnostic d = diags.get(0);
    assertTrue(d.getMessage().contains("wrong"));
    assertTrue(d.getMessage().contains("e2"));
  }

  @Test
  public void testJavaAPI16() throws IOException, InvalidUsageException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withValidator(ValidatorsFactory.getLimitedValidator());

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      assertTrue(res.isError());
      assertFalse(res.isProcessingError());
      assertTrue(res.isValidationError());

      java.util.List<Diagnostic> diags = res.getDiagnostics();
      assertEquals(1, diags.size());
      Diagnostic d = diags.get(0);
      assertTrue(d.getMessage().contains("maxInclusive"));
      assertTrue(d.getMessage().contains("e2"));
      assertTrue(d.getMessage().contains("20"));
    }
  }

  @Test
  public void testJavaAPI17() throws IOException, InvalidUsageException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = dp.withValidator(ValidatorsFactory.getXercesValidator(dp.getMainSchemaURIForFullValidation().toString()));

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      assertTrue(res.isError());
      assertFalse(res.isProcessingError());
      assertTrue(res.isValidationError());
      long actualLength = res.location().bytePos1b() - 1;
      assertEquals(file.length(), actualLength);

      java.util.List<Diagnostic> diags = res.getDiagnostics();
      assertEquals(2, diags.size());
      Diagnostic d0 = diags.get(0);
      Diagnostic d1 = diags.get(1);

      assertTrue(d0.getMessage().contains("42"));
      assertTrue(d0.getMessage().contains("e2"));
      assertTrue(d0.getMessage().contains("not valid"));

      assertTrue(d1.getMessage().contains("42"));
      assertTrue(d1.getMessage().contains("maxInclusive"));
      assertTrue(d1.getMessage().contains("20"));
    }
  }

  @Test
  public void testJavaAPI18() throws IOException, ClassNotFoundException {
    // Demonstrate that we can use the API to continue a parse where we left off
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, "e4", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData2.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream input = Infoset.getInputSourceDataInputStream(fis)) {

      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = null;
      boolean err = false;

      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertEquals(5, res.location().bytePos1b());
      assertEquals("data", outputter.getResult().getRootElement().getText());

      outputter.reset();
      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertEquals(9, res.location().bytePos1b());
      assertEquals("left", outputter.getResult().getRootElement().getText());

      outputter.reset();
      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertFalse(input.hasData());
      assertEquals(13, res.location().bytePos1b());
      assertEquals("over", outputter.getResult().getRootElement().getText());
    }
  }

  @Test
  public void testJavaAPI19() throws IOException, ClassNotFoundException {
    // Demonstrate that we cannot use the API to continue a parse with an invalid InputSource
    // ie. after a runtime SDE. This test needs to be run with an input file larger than 256MB
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/ambig_elt.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, "root", null);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myData19.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream input = Infoset.getInputSourceDataInputStream(fis)) {

      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = null;
      boolean err = false;

      res = dp.parse(input, outputter);
      err = res.isError();
      assertTrue(err);

      outputter.reset();
      try {
        res = dp.parse(input, outputter);

      } catch (Exception e) {
        assertTrue(e.getMessage().contains("Usage error"));
        assertTrue(e.getMessage().contains("invalid input source"));
      }
    }
  }

  @Test
  public void testJavaAPI20() throws Exception {
    // Test SAX parsing/unparsing
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    DaffodilParseXMLReader parseXMLReader = dp.newXMLReaderInstance();

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fisDP = new java.io.FileInputStream(file);
    java.io.FileInputStream fisSAX = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream disDP = Infoset.getInputSourceDataInputStream(fisDP)) {
      InputSourceDataInputStream disSAX = Infoset.getInputSourceDataInputStream(fisSAX);
      ByteArrayOutputStream xmlBos = new ByteArrayOutputStream();
      InfosetOutputter outputter = Infoset.getXMLTextInfosetOutputter(xmlBos, true);
      ParseResult res = dp.parse(disDP, outputter);
      String infosetDPString = xmlBos.toString();

      org.jdom2.input.sax.SAXHandler contentHandler = new org.jdom2.input.sax.SAXHandler();
      SAXErrorHandlerForAPITest errorHandler = new SAXErrorHandlerForAPITest();
      // since SAXHandler uses a blank prefix when the below isn't set to true, it introduces
      // an undesired no-prefixed xmlns mapping
      parseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true);
      parseXMLReader.setContentHandler(contentHandler);
      parseXMLReader.setErrorHandler(errorHandler);
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY,
          Paths.get(System.getProperty("java.io.tmpdir")));
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX,
          "daffodil-sapi-");
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX,
          ".sax.blob");
      parseXMLReader.parse(disSAX);
      ParseResult resSAX = (ParseResult) parseXMLReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT);
      boolean err = errorHandler.isError();
      ArrayList<Diagnostic> diags = errorHandler.getDiagnostics();
      Format pretty = Format.getPrettyFormat().setLineSeparator(System.getProperty("line.separator"));
      String infosetSAXString = new org.jdom2.output.XMLOutputter(pretty).outputString(contentHandler.getDocument());

      assertFalse(err);
      assertTrue(diags.isEmpty());
      assertEquals(infosetDPString, infosetSAXString);

      // test unparse
      ByteArrayOutputStream unparseBos = new java.io.ByteArrayOutputStream();
      WritableByteChannel wbc = java.nio.channels.Channels.newChannel(unparseBos);

      // prep for SAX unparse
      DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(wbc);
      try {
        org.xml.sax.XMLReader unparseXMLReader = javax.xml.parsers.SAXParserFactory.newInstance()
            .newSAXParser().getXMLReader();
        setSecureDefaults(unparseXMLReader);
        unparseXMLReader.setContentHandler(unparseContentHandler);
        unparseXMLReader.setErrorHandler(errorHandler);
        unparseXMLReader.setFeature(SAX_NAMESPACES_FEATURE, true);
        unparseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true);
        ByteArrayInputStream is = new ByteArrayInputStream(infosetSAXString.getBytes());
        // kickstart unparse
        unparseXMLReader.parse(new org.xml.sax.InputSource(is));
      } catch (javax.xml.parsers.ParserConfigurationException | org.xml.sax.SAXException e) {
        fail("Error: " + e);
      }

      UnparseResult saxUr = unparseContentHandler.getUnparseResult();
      wbc.close();

      boolean saxErr = saxUr.isError();
      assertFalse(saxErr);
      assertTrue(saxUr.getDiagnostics().isEmpty());
      assertEquals("42", unparseBos.toString());
    }
  }

  @Test
  public void testJavaAPI21() throws IOException, ClassNotFoundException {
    // Test SAX parsing with errors
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    DaffodilParseXMLReader parseXMLReader = dp.newXMLReaderInstance();

    java.io.File file = getResource("/test/api/myDataBroken.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {

      org.jdom2.input.sax.SAXHandler contentHandler = new org.jdom2.input.sax.SAXHandler();
      SAXErrorHandlerForAPITest errorHandler = new SAXErrorHandlerForAPITest();
      parseXMLReader.setContentHandler(contentHandler);
      parseXMLReader.setErrorHandler(errorHandler);
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY,
          Paths.get(System.getProperty("java.io.tmpdir")));
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX, "daffodil-sapi-");
      parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX, ".sax.blob");
      parseXMLReader.parse(dis);
      boolean err = errorHandler.isError();
      ArrayList<Diagnostic> diags = errorHandler.getDiagnostics();

      assertTrue(err);
      assertEquals(1, diags.size());
      Diagnostic d = diags.get(0);
      assertTrue(d.getMessage().contains("int"));
      assertTrue(d.getMessage().contains("Not an int"));
      assertTrue(d.getDataLocations().toString().contains("10"));
      java.util.List<LocationInSchemaFile> locs = d.getLocationsInSchemaFiles();
      assertEquals(1, locs.size());
      LocationInSchemaFile loc = locs.get(0);
      assertTrue(loc.asString().contains("mySchema1.dfdl.xsd"));
    }
  }

  @Test
  public void testJavaAPI22_withExternalVariablesUsingAbstractMap() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates here that we can set external variables using a
    // Java AbstractMap after compilation but before parsing via DataProcessor.
    DebuggerRunnerForAPITest debugger = new DebuggerRunnerForAPITest();

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchemaWithVars.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);
    dp = dp.withDebuggerRunner(debugger);
    dp = dp.withDebugging(true);

    java.util.AbstractMap<String, String> extVarsMap = new java.util.HashMap<String, String>();
    extVarsMap.put("var1", "var1ValueFromMap");

    dp = dp.withExternalVariables(extVarsMap);

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      org.jdom2.Document doc = outputter.getResult();
      org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
      xo.setFormat(Format.getPrettyFormat());
      String docString = xo.outputString(doc);
      boolean containsVar1 = docString.contains("var1Value");
      boolean containsVar1Value = docString.contains("var1ValueFromMap");
      assertTrue(containsVar1);
      assertTrue(containsVar1Value);

      assertTrue(debugger.lines.size() > 0);
      assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
    }
  }

  @Test
  public void testJavaAPI23() throws IOException, ClassNotFoundException {
    // test SAX unparsing with errors
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.io.File file = getResource("/test/api/myInfosetBroken.xml");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
    WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
    // prep for SAX
    DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(wbc);
    try {
      org.xml.sax.XMLReader unparseXMLReader = javax.xml.parsers.SAXParserFactory.newInstance()
          .newSAXParser().getXMLReader();
      setSecureDefaults(unparseXMLReader);
      unparseXMLReader.setContentHandler(unparseContentHandler);
      unparseXMLReader.setFeature(SAX_NAMESPACES_FEATURE, true);
      unparseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true);
      // kickstart unparse
      unparseXMLReader.parse(new org.xml.sax.InputSource(fis));
    } catch (DaffodilUnparseErrorSAXException | DaffodilUnhandledSAXException ignored) {
      // do nothing; UnparseError is handled below while we don't expect Unhandled in this test
    } catch (javax.xml.parsers.ParserConfigurationException | org.xml.sax.SAXException e) {
      fail("Error: " + e);
    }

    UnparseResult res = unparseContentHandler.getUnparseResult();
    boolean err = res.isError();
    assertTrue(err);

    java.util.List<Diagnostic> diags = res.getDiagnostics();
    assertEquals(1, diags.size());
    Diagnostic d = diags.get(0);
    assertTrue(d.getMessage().contains("wrong"));
    assertTrue(d.getMessage().contains("e2"));
  }

  @Test
  public void testJavaAPI24() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates error cases of setting external variables
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    java.io.File schemaFile = getResource("/test/api/mySchemaWithComplexVars1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = reserializeDataProcessor(dp);

    java.util.AbstractMap<String, String> extVarsMap = new java.util.HashMap<String, String>();

    // set var without a namespace, ambiguity error because schema contains
    // two variables with same name but different namespace
    extVarsMap.clear();
    extVarsMap.put("var", "10");
    try {
      dp = dp.withExternalVariables(extVarsMap);
    } catch (ExternalVariableException e) {
      String msg = e.getMessage();
      assertTrue(msg.contains("var"));
      assertTrue(msg.contains("ambiguity"));
      assertTrue(msg.contains("ex1:var"));
      assertTrue(msg.contains("ex2:var"));
    }

    // variable without namespace does not exist error
    extVarsMap.clear();
    extVarsMap.put("dne", "10");
    try {
      dp = dp.withExternalVariables(extVarsMap);
    } catch (ExternalVariableException e) {
      String msg = e.getMessage();
      assertTrue(msg.contains("definition not found"));
      assertTrue(msg.contains("dne"));
    }

    // variable with namespace does not exist error
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/1}dne", "10");
    try {
      dp = dp.withExternalVariables(extVarsMap);
    } catch (ExternalVariableException e) {
      String msg = e.getMessage();
      assertTrue(msg.contains("definition not found"));
      assertTrue(msg.contains("{http://example.com/1}dne"));
    }

    // variable cannot be set externally
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/2}var", "10");
    try {
      dp = dp.withExternalVariables(extVarsMap);
    } catch (ExternalVariableException e) {
      String msg = e.getMessage();
      assertTrue(msg.contains("ex2:var"));
      assertTrue(msg.contains("cannot be set externally"));
    }

    // variable not valid with regards to type
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/1}var", "notAnInt");
    try {
      dp = dp.withExternalVariables(extVarsMap);
    } catch (ExternalVariableException e) {
      String msg = e.getMessage();
      assertTrue(msg.contains("ex1:var"));
      assertTrue(msg.contains("is not a valid xs:int"));
      assertTrue(msg.contains("notAnInt"));
    }

    // can change the value of the same variable multiple times
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/1}var", "100");
    dp = dp.withExternalVariables(extVarsMap);
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/1}var", "200");
    dp = dp.withExternalVariables(extVarsMap);

    // can parse with the variable values
    {
      byte[] ba = {};
      ByteBuffer bb = ByteBuffer.wrap(ba);
      try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(bb)) {
        JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        assertFalse(res.isError());
        org.jdom2.Document doc = outputter.getResult();
        org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
        xo.setFormat(Format.getPrettyFormat());
        String docString = xo.outputString(doc);
        assertTrue(docString.contains("<ex1var>200</ex1var>"));
      }
    }

    // can set an external variable after a parse
    extVarsMap.clear();
    extVarsMap.put("{http://example.com/1}var", "300");
    dp = dp.withExternalVariables(extVarsMap);

    // can parse with the updated variable value
    {
      byte[] ba = {};
      ByteBuffer bb = ByteBuffer.wrap(ba);
      try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(bb)) {
        JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        assertFalse(res.isError());
        org.jdom2.Document doc = outputter.getResult();
        org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
        xo.setFormat(Format.getPrettyFormat());
        String docString = xo.outputString(doc);
        assertTrue(docString.contains("<ex1var>300</ex1var>"));
      }
    }
  }

  @Test
  public void testJavaAPI25() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates the use of a custom InfosetInputter/Outputter

    String expectedData = "42";
    TestInfosetEvent expectedEvents[] = {
        TestInfosetEvent.startDocument(),
        TestInfosetEvent.startComplex("e1", "http://example.com"),
        TestInfosetEvent.startSimple("e2", "http://example.com", expectedData),
        TestInfosetEvent.endSimple("e2", "http://example.com"),
        TestInfosetEvent.endComplex("e1", "http://example.com"),
        TestInfosetEvent.endDocument()
    };

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      TestInfosetOutputter outputter = new TestInfosetOutputter();
      ParseResult pr = dp.parse(dis, outputter);

      assertFalse(pr.isError());
      assertArrayEquals(expectedEvents, outputter.events.toArray());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      TestInfosetInputter inputter = new TestInfosetInputter(expectedEvents);
      UnparseResult ur = dp.unparse(inputter, wbc);

      assertFalse(ur.isError());
      assertEquals(expectedData, bos.toString());
    }
  }

  @Test
  public void testJavaAPI26() throws IOException, ClassNotFoundException, ExternalVariableException {
    // Demonstrates the use of the various EntityResolver methods from Java

    assertTrue(DaffodilXMLEntityResolver.getEntityResolver() != null);
    assertTrue(DaffodilXMLEntityResolver.getXMLEntityResolver() != null);
    assertTrue(DaffodilXMLEntityResolver.getLSResourceResolver() != null);
  }

  @Test
  public void testJavaAPI27() throws IOException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema6.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    assertTrue(pf.isError());
    try {
      pf.onPath("/");
    } catch (Exception e) {
      Throwable cause = e.getCause();
      assertTrue(cause.toString().contains("Must call isError"));
      assertTrue(cause.getCause().toString().contains("Schema Definition Error"));
      assertTrue(cause.getCause().toString().contains("tns:nonExistent"));
    }
  }

  @Test
  public void testJavaAPINullXMLTextEscapeStyle() throws IOException, ClassNotFoundException {
    ByteArrayOutputStream xmlBos = new ByteArrayOutputStream();
    try {
      InfosetOutputter outputter = Infoset.getXMLTextInfosetOutputter(xmlBos, true, null);
    } catch (Exception e) {
      String msg = e.getMessage().toLowerCase();
      assertTrue(msg.contains("unrecognized"));
      assertTrue(msg.contains("null"));
      assertTrue(msg.contains("xmltextescapestyle"));
    }
  }

  @Test
  public void testJavaAPICDATA1() throws Exception {
    String expected = "NO_WHITESPACE_OR_SPECIAL_CHARS";
    String data = "NO_WHITESPACE_OR_SPECIAL_CHARS$";
    String schemaType = "string";
    doXMLTextEscapeStyleTest(expected, data, schemaType);
  }

  @Test
  public void testJavaAPICDATA2() throws Exception {
    String expected = "<![CDATA[   'some' stuff   here &#xE000; and ]]]]><![CDATA[> even]]>";
    String data = "   'some' stuff   here &#xE000; and ]]> even$";
    String schemaType = "string";
    doXMLTextEscapeStyleTest(expected, data, schemaType);
  }

  @Test
  public void testJavaAPICDATA3() throws Exception {
    String expected = "6.892";
    String data = "6.892";
    String schemaType = "float";
    doXMLTextEscapeStyleTest(expected, data, schemaType);
  }

  @Test
  public void testJavaAPICDATA4() throws Exception {
    String expected = "<![CDATA[this contains a CRLF\nline ending]]>";
    String data = "this contains a CRLF\r\nline ending$";
    String schemaType = "string";
    doXMLTextEscapeStyleTest(expected, data, schemaType);
  }

  @Test
  public void testJavaAPICDATA5() throws Exception {
    String expected = "<![CDATA[abcd&gt]]>";
    String data = "abcd&gt$";
    String schemaType = "string";
    doXMLTextEscapeStyleTest(expected, data, schemaType);
  }

  public void doXMLTextEscapeStyleTest(String expect, String data, String schemaType)
      throws Exception {

    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchemaCDATA.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile, schemaType, null);
    DataProcessor dp = pf.onPath("/");

    ByteArrayInputStream is = new ByteArrayInputStream(data.getBytes(StandardCharsets.UTF_8));
    try (InputSourceDataInputStream input = Infoset.getInputSourceDataInputStream(is)) {
      ByteArrayOutputStream bosDP = new ByteArrayOutputStream();
      InfosetOutputter outputter = Infoset.getXMLTextInfosetOutputter(bosDP, true, XMLTextEscapeStyle.CDATA);
      ParseResult res = dp.parse(input, outputter);
      boolean err = res.isError();

      String infosetDPString = bosDP.toString();
      int start = infosetDPString.indexOf(".com\">") + 6;
      int end = infosetDPString.indexOf("</tns");
      String value = infosetDPString.substring(start, end);

      assertFalse(err);
      assertEquals(expect, value);
    }
  }

  @Test
  public void testJavaAPIBlob1() throws Exception {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/blob.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");
    dp = dp.withValidator(ValidatorsFactory.getXercesValidator(dp.getMainSchemaURIForFullValidation().toString()));

    byte[] data = new byte[]{0x00, 0x00, 0x00, 0x04, 0x01, 0x02, 0x03, 0x04};
    ByteArrayInputStream bis = new ByteArrayInputStream(data);
    try (InputSourceDataInputStream input = Infoset.getInputSourceDataInputStream(data)) {

      Path blobRoot = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil", "api");
      Files.createDirectories(blobRoot);
      Path blobDir = Files.createTempDirectory(blobRoot, "blob-");

      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      InfosetOutputter output = Infoset.getXMLTextInfosetOutputter(bos, true);
      output.setBlobAttributes(blobDir, "pre-", ".suf");

      ParseResult res = dp.parse(input, output);
      List<Path> blobPaths = output.getBlobPaths();

      try {
        assertFalse(res.isError());
        assertTrue(blobPaths.size() == 1);
        System.out.println(blobPaths.get(0));
        assertTrue(blobPaths.get(0).toString().contains("blob-"));
        assertTrue(blobPaths.get(0).toString().contains("pre-"));
        assertTrue(blobPaths.get(0).toString().contains(".suf"));
      } finally {
        Iterator<Path> pathIter = blobPaths.iterator();
        while (pathIter.hasNext()) Files.delete(pathIter.next());
        Files.delete(blobDir);
      }
    }
  }

  /**
   * Verify that ProcessorFactory.withDistinguishedRootNode selects the right node
   */
  @Test
  public void testJavaAPIWithDistinguishedRootNode() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    // e3 is defined first in mySchema3.dfdl.xsd, so if withDistinguishedRootNode is ignored,
    // this should give a different result
    java.io.File schemaFile = getResource("/test/api/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    pf = pf.withDistinguishedRootNode("e4", null);
    DataProcessor dp = pf.onPath("/");

    java.io.File file = getResource("/test/api/myData16.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      boolean err = res.isError();
      assertFalse(err);
      assertEquals(5, res.location().bytePos1b());
      assertEquals(33, res.location().bitPos1b());

      java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
      java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
      InfosetInputter inputter = Infoset.getJDOMInfosetInputter(outputter.getResult());
      UnparseResult res2 = dp.unparse(inputter, wbc);
      err = res2.isError();
      assertFalse(err);
      assertEquals("9100", bos.toString());
    }
  }

  /***
   * Verify that a user can get diagnostics without having to call isError
   *
   * @throws IOException
   */
  @Test
  public void testJavaAPIGetDiagnostics() throws IOException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = new java.io.File("/test/api/notHere1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    List<Diagnostic> diags = pf.getDiagnostics();
    boolean found1 = false;
    for (Diagnostic d : diags) {
      if (d.getMessage().contains("notHere1")) {
        found1 = true;
      }
    }
    assertTrue(found1);
    assertTrue(pf.isError());
  }

  @Test
  public void testJavaAPICompileResource() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    String name = "/test/api/mySchema1.dfdl.xsd";
    ProcessorFactory pf = c.compileResource(name);
    DataProcessor dp = pf.onPath("/");

    java.io.File file = getResource("/test/api/myDataBroken.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      assertTrue(res.isError());

      Diagnostic d = res.getDiagnostics().get(0);
      LocationInSchemaFile loc = d.getLocationsInSchemaFiles().get(0);
      assertTrue(loc.asString().replace("\\", "/").contains("in " + name));
    }
  }

  @Test
  public void testJavaAPICompileSource1() throws IOException, URISyntaxException, InvalidUsageException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    URI uri = new URI("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileSource(uri);
    DataProcessor dp1 = pf.onPath("/");
    DataProcessor dp = dp1.withValidator(ValidatorsFactory.getXercesValidator(dp1.getMainSchemaURIForFullValidation().toString()));

    java.io.File file = getResource("/test/api/myDataBroken.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      assertTrue(res.isError());

      Diagnostic d = res.getDiagnostics().get(0);
      LocationInSchemaFile loc = d.getLocationsInSchemaFiles().get(0);
      assertTrue(loc.asString().replace("\\", "/").contains("in " + uri.getPath()));
    }
  }

  // intended to test the case where compileSource succeeds, but onPath
  // can't find the file when it tries to resolve the schemaLocation
  // takes care of coverage for this case
  @Test
  public void testJavaAPICompileSource2() throws IOException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    File tempFile = File.createTempFile("testJavaAPI", ".schema");
    File schemaFile = getResource("/test/api/mySchema2.dfdl.xsd");
    FileUtils.copyFile(schemaFile, tempFile);
    ProcessorFactory pf = c.compileSource(tempFile.toURI());
    try {
      assertFalse(pf.isError());
      // delete file needed by Xerces for full validation
      tempFile.delete();
      // should throw FileNotFoundException because onPath calls resolveSchemaLocation
      // on the URI backed by the deleted file
      pf.onPath("/");
      // fail if exception was not thrown
      fail();
    } catch (Exception e) {
      assertTrue(e.getMessage().contains("Could not find file or resource"));
    } finally {
      if (tempFile.exists()) tempFile.delete();
    }
  }

  @Test
  public void testJavaAPIJson1() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis);
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    InfosetOutputter outputter = Infoset.getJsonInfosetOutputter(bos, false);
    ParseResult res = dp.parse(dis, outputter);
    assertFalse(res.isError());

    java.io.ByteArrayInputStream input = new java.io.ByteArrayInputStream(bos.toByteArray());
    InfosetInputter inputter = Infoset.getJsonInfosetInputter(input);
    java.io.ByteArrayOutputStream bos2 = new java.io.ByteArrayOutputStream();
    java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos2);
    UnparseResult res2 = dp.unparse(inputter, wbc);
    assertFalse(res2.isError());
    assertEquals("42", bos2.toString());
  }

  @Test
  public void testJavaAPIJson2() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/");

    // e2 should be a simple type
    String badJsonInfoset = "{\"e1\": {\"e2\": {\"unexpected\": \"object\"}}}";

    java.io.ByteArrayInputStream input = new java.io.ByteArrayInputStream(badJsonInfoset.getBytes("UTF-8"));
    InfosetInputter inputter = Infoset.getJsonInfosetInputter(input);
    java.io.ByteArrayOutputStream bos2 = new java.io.ByteArrayOutputStream();
    java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos2);
    UnparseResult res = dp.unparse(inputter, wbc);
    assertTrue(res.isError());
    java.util.List<Diagnostic> diags = res.getDiagnostics();
    assertEquals(1, diags.size());
    assertTrue(diags.get(0).toString().contains("Illegal content for simple element"));
    assertTrue(diags.get(0).toString().contains("Unexpected array or object"));
    assertTrue(diags.get(0).toString().contains("e2"));
  }

}
