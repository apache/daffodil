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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.daffodil.japi.*;
import org.apache.daffodil.japi.infoset.XMLTextInfosetOutputter;
import org.jdom2.output.Format;
import org.junit.Test;

import org.apache.daffodil.japi.infoset.JDOMInfosetInputter;
import org.apache.daffodil.japi.infoset.JDOMInfosetOutputter;
import org.apache.daffodil.japi.logger.ConsoleLogWriter;
import org.apache.daffodil.japi.logger.LogLevel;
import org.apache.daffodil.japi.io.InputSourceDataInputStream;

public class TestJavaAPI {

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
     *
     * All of the JAPI tests create a DataProcessor. To test that we correctly
     * made all the necessary changes to make the JAPI DataProcessor
     * serializable, it is important to serialize and deserialize that
     * DataProcessor before use in the tests. This function acts as a helper
     * function to accomplish that task.
     *
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
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withDebuggerRunner(debugger);
        dp = dp.withDebugging(true);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());
        assertEquals(0, lw.errors.size());
        assertEquals(0, lw.warnings.size());
        assertTrue(lw.others.size() > 0);
        assertTrue(debugger.lines.size() > 0);
        assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
        assertTrue(debugger.getCommand().equals("trace"));

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("42", bos.toString());

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    // This is a duplicate of test testJavaAPI1 that serializes the parser
    // before executing the test.
    @Test
    public void testJavaAPI1_A() throws Exception {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");

        // Serialize the parser to memory, then deserialize for parsing.
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        WritableByteChannel output = Channels.newChannel(os);
        dp.save(output);

        ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
        ReadableByteChannel input = Channels.newChannel(is);
        org.apache.daffodil.japi.Compiler compiler = Daffodil.compiler();
        DataProcessor parser = compiler.reload(input);
        parser = parser.withDebuggerRunner(debugger);
        parser = parser.withDebugging(true);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = parser.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());
        assertEquals(0, lw.errors.size());
        assertEquals(0, lw.warnings.size());
        assertTrue(lw.others.size() > 0);
        assertTrue(debugger.lines.size() > 0);
        assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));
        assertTrue(debugger.getCommand().equals("trace"));

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("42", bos.toString());

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    // This is a duplicate of test testJavaAPI1 that serializes the parser
    // before executing the test.
    @Test
    public void testJavaAPI1_A_FullFails() throws Exception {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
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
        org.apache.daffodil.japi.Compiler compiler = Daffodil.compiler();
        DataProcessor parser = compiler.reload(input);

        try {
            parser = parser.withValidationMode(ValidationMode.Full);
            fail();
        } catch (InvalidUsageException e) {
            assertEquals("'Full' validation not allowed when using a restored parser.", e.getMessage());
        }

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI2() throws IOException, ClassNotFoundException {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Info);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myDataBroken.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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
        assertTrue(loc.toString().contains("mySchema1.dfdl.xsd"));
        // above is mySchema1 as it reports the element ref location,
        // not element decl.

        assertEquals(0, lw.errors.size());
        assertEquals(0, lw.warnings.size());
        // assertTrue(lw.infos.size() > 0);
        assertEquals(0, lw.others.size());

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    /**
     * Verify that we can detect when the parse did not consume all the data.
     * 
     * @throws IOException
     */
    @Test
    public void testJavaAPI3() throws IOException, ClassNotFoundException {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema3.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        pf = pf.withDistinguishedRootNode("e3", null);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myData16.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertFalse(res.location().isAtEnd());
        assertEquals(2, res.location().bytePos1b());
        assertEquals(9, res.location().bitPos1b());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("9", bos.toString());
    }

    // This is a duplicate of test testJavaAPI3 that serializes the parser
    // before executing the test.
    @Test
    public void testJavaAPI3_A() throws Exception {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema3.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        pf = pf.withDistinguishedRootNode("e3", null);
        DataProcessor dp = pf.onPath("/");

        // Serialize the parser to memory, then deserialize for parsing.
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        WritableByteChannel output = Channels.newChannel(os);
        dp.save(output);

        ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
        ReadableByteChannel input = Channels.newChannel(is);
        org.apache.daffodil.japi.Compiler compiler = Daffodil.compiler();
        DataProcessor parser = compiler.reload(input);

        java.io.File file = getResource("/test/japi/myData16.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = parser.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertFalse(res.location().isAtEnd());
        assertEquals(2, res.location().bytePos1b());
        assertEquals(9, res.location().bitPos1b());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("9", bos.toString());
    }

    @Test
    public void testJavaAPI4b() throws IOException, ClassNotFoundException {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        File schemaFileName = getResource("/test/japi/mySchema3.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFileName, "e4", null);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myData2.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertFalse(res.location().isAtEnd());
        assertEquals(5, res.location().bytePos1b());
        assertEquals(33, res.location().bitPos1b());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("data", bos.toString());
    }

    @Test
    public void testJavaAPI5() throws IOException, ClassNotFoundException {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        File schemaFileName = getResource("/test/japi/mySchema3.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFileName, "e4", null); // e4 is a 4-byte long string
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myData3.dat"); // contains 5
                                                                   // bytes
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(!res.location().isAtEnd());
        assertEquals(5, res.location().bytePos1b());
        assertEquals(33, res.location().bitPos1b());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertEquals("data", bos.toString());
    }

    /***
     * Verify that the compiler throws a FileNotFound exception when fed a list
     * of schema files that do not exist.
     * 
     * @throws IOException
     */
    @Test
    public void testJavaAPI6() throws IOException {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = new java.io.File("/test/japi/notHere1.dfdl.xsd");
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

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
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
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/TopLevel.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel", null);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/01very_simple.txt");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"));

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
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
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/TopLevel.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel2", null);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/01very_simple.txt");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(outputter.getResult());
        UnparseResult res2 = dp.unparse(inputter, wbc);
        err = res2.isError();
        assertFalse(err);
        assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"));

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    /**
     * Verify that calling result() on the ParseResult multiple times does not
     * error.
     */
    @Test
    public void testJavaAPI9() throws IOException, ClassNotFoundException {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/TopLevel.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile, "TopLevel2", null);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/01very_simple.txt");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());

        org.jdom2.Document doc1 = outputter.getResult();

        java.io.ByteArrayOutputStream bos1 = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc1 = java.nio.channels.Channels.newChannel(bos1);
        JDOMInfosetInputter inputter1 = new JDOMInfosetInputter(doc1);
        UnparseResult res2 = dp.unparse(inputter1, wbc1);
        err = res2.isError();
        assertFalse(err);
        assertTrue(bos1.toString().contains("Return-Path: <bob@smith.com>"));

        org.jdom2.Document doc2 = outputter.getResult();

        java.io.ByteArrayOutputStream bos2 = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc2 = java.nio.channels.Channels.newChannel(bos2);
        JDOMInfosetInputter inputter2 = new JDOMInfosetInputter(doc2);
        UnparseResult res3 = dp.unparse(inputter2, wbc2);
        err = res3.isError();
        assertFalse(err);
        assertTrue(bos2.toString().contains("Return-Path: <bob@smith.com>"));

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    /**
     * Verify that hidden elements do not appear in the resulting infoset
     */
    @Test
    public void testJavaAPI10() throws IOException, ClassNotFoundException {

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema4.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myData4.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        org.jdom2.Document doc = outputter.getResult();
        org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
        xo.setFormat(Format.getPrettyFormat());
        org.jdom2.Element rootNode = doc.getRootElement();
        org.jdom2.Element hidden = rootNode.getChild("hiddenElement", rootNode.getNamespace());
        assertTrue(null == hidden);
        assertTrue(res.location().isAtEnd());
    }

    /**
     * Verify that nested elements do not appear as duplicates
     */
    @Test
    public void testJavaAPI11() throws IOException, ClassNotFoundException {

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema5.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myData5.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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
        assertTrue(res.location().isAtEnd());
    }

    @Test
    public void testJavaAPI12() throws IOException, ClassNotFoundException {
        LogWriterForJAPITest2 lw2 = new LogWriterForJAPITest2();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw2);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();


        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withDebuggerRunner(debugger);
        dp = dp.withDebugging(true);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        boolean err = res.isError();
        assertFalse(err);
        assertTrue(res.location().isAtEnd());

        assertEquals(0, lw2.errors.size());
        assertEquals(0, lw2.warnings.size());
        assertTrue(lw2.others.size() > 0);
        assertTrue(debugger.lines.size() > 0);
        assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI13() throws IOException, ClassNotFoundException {
        // Demonstrates here that we can set external variables
        // after compilation but before parsing via Compiler.
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File extVarsFile = getResource("/test/japi/external_vars_1.xml");
        java.io.File schemaFile = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);

        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withDebuggerRunner(debugger);
        dp = dp.withDebugging(true);
        dp = dp.withExternalVariables(extVarsFile);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI14() throws IOException, ClassNotFoundException {
        // Demonstrates here that we can set external variables
        // after compilation but before parsing via DataProcessor.
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File extVarFile = getResource("/test/japi/external_vars_1.xml");
        java.io.File schemaFile = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withDebuggerRunner(debugger);
        dp = dp.withDebugging(true);
        dp = dp.withExternalVariables(extVarFile);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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
        assertTrue(res.location().isAtEnd());

        assertEquals(0, lw.errors.size());
        assertEquals(0, lw.warnings.size());
        assertTrue(lw.others.size() > 0);
        assertTrue(debugger.lines.size() > 0);
        assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI15() throws IOException, ClassNotFoundException {
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Info);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myInfosetBroken.xml");
        org.jdom2.input.SAXBuilder builder = new org.jdom2.input.SAXBuilder();

        org.jdom2.Document doc = null;
        try {
            doc = builder.build(file);
        } catch (Exception e) {
            fail();
        }

        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        JDOMInfosetInputter inputter = new JDOMInfosetInputter(doc);
        UnparseResult res = dp.unparse(inputter, wbc);
        boolean err = res.isError();
        assertTrue(err);

        java.util.List<Diagnostic> diags = res.getDiagnostics();
        assertEquals(1, diags.size());
        Diagnostic d = diags.get(0);
        assertTrue(d.getMessage().contains("wrong"));
        assertTrue(d.getMessage().contains("e2"));

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI16() throws IOException, InvalidUsageException, ClassNotFoundException {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withValidationMode(ValidationMode.Limited);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        assertTrue(res.isError());
        assertFalse(res.isProcessingError());
        assertTrue(res.isValidationError());
        assertTrue(res.location().isAtEnd());

        java.util.List<Diagnostic> diags = res.getDiagnostics();
        assertEquals(1, diags.size());
        Diagnostic d = diags.get(0);
        assertTrue(d.getMessage().contains("maxInclusive"));
        assertTrue(d.getMessage().contains("e2"));
        assertTrue(d.getMessage().contains("20"));
    }

    @Test
    public void testJavaAPI17() throws IOException, InvalidUsageException, ClassNotFoundException {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = dp.withValidationMode(ValidationMode.Full);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
        ParseResult res = dp.parse(dis, outputter);
        assertTrue(res.isError());
        assertFalse(res.isProcessingError());
        assertTrue(res.isValidationError());
        assertTrue(res.location().isAtEnd());

        java.util.List<Diagnostic> diags = res.getDiagnostics();
        assertEquals(3, diags.size());
        Diagnostic d0 = diags.get(0);
        Diagnostic d1 = diags.get(1);
        Diagnostic d2 = diags.get(2);

        assertTrue(d0.getMessage().contains("42"));
        assertTrue(d0.getMessage().contains("e2"));
        assertTrue(d0.getMessage().contains("not valid"));

        assertTrue(d1.getMessage().contains("42"));
        assertTrue(d1.getMessage().contains("maxInclusive"));
        assertTrue(d1.getMessage().contains("20"));

        assertTrue(d2.getMessage().contains("maxInclusive"));
        assertTrue(d2.getMessage().contains("e2"));
        assertTrue(d2.getMessage().contains("20"));
    }

    @Test
    public void testJavaAPI18() throws IOException, ClassNotFoundException {
      // Demonstrate that we can use the API to continue a parse where we left off
      org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

      java.io.File schemaFile = getResource("/test/japi/mySchema3.dfdl.xsd");
      ProcessorFactory pf = c.compileFile(schemaFile,"e4", null);
      DataProcessor dp = pf.onPath("/");
      dp = reserializeDataProcessor(dp);

      java.io.File file = getResource("/test/japi/myData2.dat");
      java.io.FileInputStream fis = new java.io.FileInputStream(file);
      InputSourceDataInputStream input = new InputSourceDataInputStream(fis);

      JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
      ParseResult res = null;
      boolean err = false;

      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertFalse(res.location().isAtEnd());
      assertEquals(5, res.location().bytePos1b());
      assertEquals("data", outputter.getResult().getRootElement().getText());

      outputter.reset();
      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertFalse(res.location().isAtEnd());
      assertEquals(9, res.location().bytePos1b());
      assertEquals("left", outputter.getResult().getRootElement().getText());

      outputter.reset();
      res = dp.parse(input, outputter);
      err = res.isError();
      assertFalse(err);
      assertTrue(res.location().isAtEnd());
      assertEquals(13, res.location().bytePos1b());
      assertEquals("over", outputter.getResult().getRootElement().getText());
    }

    @Test
    public void testJavaAPI19() throws IOException, ClassNotFoundException {
      // Demonstrate that we cannot use the API to continue a parse with an invalid InputSource
      // ie. after a runtime SDE. This test needs to be run with an input file larger than 256MB
      org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

      java.io.File schemaFile = getResource("/test/japi/ambig_elt.dfdl.xsd");
      ProcessorFactory pf = c.compileFile(schemaFile, "root", null);
      DataProcessor dp = pf.onPath("/");
      dp = reserializeDataProcessor(dp);

      java.io.File file = getResource("/test/japi/myData19.dat");
      java.io.FileInputStream fis = new java.io.FileInputStream(file);
      InputSourceDataInputStream input = new InputSourceDataInputStream(fis);

      JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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

    @Test
    public void testJavaAPI20() throws IOException, ClassNotFoundException {
        // Test SAX parsing/unparsing
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        DaffodilParseXMLReader parseXMLReader = dp.newXMLReaderInstance();

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fisDP = new java.io.FileInputStream(file);
        java.io.FileInputStream fisSAX = new java.io.FileInputStream(file);
        InputSourceDataInputStream disDP = new InputSourceDataInputStream(fisDP);
        InputSourceDataInputStream disSAX = new InputSourceDataInputStream(fisSAX);
        ByteArrayOutputStream xmlBos = new ByteArrayOutputStream();
        XMLTextInfosetOutputter outputter = new XMLTextInfosetOutputter(xmlBos, true);
        ParseResult res = dp.parse(disDP, outputter);
        String infosetDPString = xmlBos.toString();

        org.jdom2.input.sax.SAXHandler contentHandler = new org.jdom2.input.sax.SAXHandler();
        SAXErrorHandlerForJAPITest errorHandler = new SAXErrorHandlerForJAPITest();
        parseXMLReader.setContentHandler(contentHandler);
        parseXMLReader.setErrorHandler(errorHandler);
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY(),
                Paths.get(System.getProperty("java.io.tmpdir")));
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX(),
                "daffodil-sapi-");
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX(),
                ".sax.blob");
        parseXMLReader.parse(disSAX);
        ParseResult resSAX = (ParseResult) parseXMLReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
        boolean err = errorHandler.isError();
        ArrayList<Diagnostic> diags = errorHandler.getDiagnostics();
        Format pretty = Format.getPrettyFormat().setLineSeparator(System.getProperty("line.separator"));
        String infosetSAXString = new  org.jdom2.output.XMLOutputter(pretty).outputString(contentHandler.getDocument());

        assertFalse(err);
        assertTrue(resSAX.location().isAtEnd());
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


    @Test
    public void testJavaAPI21() throws IOException, ClassNotFoundException {
        // Test SAX parsing with errors
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        DaffodilParseXMLReader parseXMLReader = dp.newXMLReaderInstance();

        java.io.File file = getResource("/test/japi/myDataBroken.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);

        org.jdom2.input.sax.SAXHandler contentHandler = new org.jdom2.input.sax.SAXHandler();
        SAXErrorHandlerForJAPITest errorHandler = new SAXErrorHandlerForJAPITest();
        parseXMLReader.setContentHandler(contentHandler);
        parseXMLReader.setErrorHandler(errorHandler);
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY(),
                Paths.get(System.getProperty("java.io.tmpdir")));
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX(), "daffodil-sapi-");
        parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX(), ".sax.blob");
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
        assertTrue(loc.toString().contains("mySchema1.dfdl.xsd"));
    }

    @Test
    public void testJavaAPI22_setExternalVariablesUsingAbstractMap() throws IOException, ClassNotFoundException {
        // Demonstrates here that we can set external variables using a
        // Java AbstractMap after compilation but before parsing via DataProcessor.
        LogWriterForJAPITest lw = new LogWriterForJAPITest();
        DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Debug);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File extVarFile = getResource("/test/japi/external_vars_1.xml");
        java.io.File schemaFile = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);
        dp = dp.withDebuggerRunner(debugger);
        dp = dp.withDebugging(true);

        java.util.AbstractMap<String, String> extVarsMap = new java.util.HashMap<String, String>();
        extVarsMap.put("var1", "var1ValueFromMap");

        dp = dp.withExternalVariables(extVarsMap);

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
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

        assertTrue(res.location().isAtEnd());

        assertEquals(0, lw.errors.size());
        assertEquals(0, lw.warnings.size());
        assertTrue(lw.others.size() > 0);
        assertTrue(debugger.lines.size() > 0);
        assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));

        // reset the global logging and debugger state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }

    @Test
    public void testJavaAPI23() throws IOException, ClassNotFoundException {
        // test SAX unparsing with errors
        LogWriterForJAPITest lw = new LogWriterForJAPITest();

        Daffodil.setLogWriter(lw);
        Daffodil.setLoggingLevel(LogLevel.Info);

        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        DataProcessor dp = pf.onPath("/");
        dp = reserializeDataProcessor(dp);

        java.io.File file = getResource("/test/japi/myInfosetBroken.xml");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
        // prep for SAX
        DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(wbc);
        try {
            org.xml.sax.XMLReader unparseXMLReader = javax.xml.parsers.SAXParserFactory.newInstance()
                    .newSAXParser().getXMLReader();
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

        // reset the global logging state
        Daffodil.setLogWriter(new ConsoleLogWriter());
        Daffodil.setLoggingLevel(LogLevel.Info);
    }
}
