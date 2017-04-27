/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.example;

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
import java.util.List;

import org.jdom2.output.Format;
import org.junit.Test;

import edu.illinois.ncsa.daffodil.japi.Daffodil;
import edu.illinois.ncsa.daffodil.japi.DataProcessor;
import edu.illinois.ncsa.daffodil.japi.Diagnostic;
import edu.illinois.ncsa.daffodil.japi.InvalidUsageException;
import edu.illinois.ncsa.daffodil.japi.LocationInSchemaFile;
import edu.illinois.ncsa.daffodil.japi.ParseResult;
import edu.illinois.ncsa.daffodil.japi.ProcessorFactory;
import edu.illinois.ncsa.daffodil.japi.UnparseResult;
import edu.illinois.ncsa.daffodil.japi.ValidationMode;
import edu.illinois.ncsa.daffodil.japi.logger.ConsoleLogWriter;
import edu.illinois.ncsa.daffodil.japi.logger.LogLevel;
import edu.illinois.ncsa.daffodil.japi.infoset.JDOMInfosetOutputter;

public class TestJavaAPI {

	public java.io.File getResource(String resPath) {
		try {
			return new java.io.File(this.getClass().getResource(resPath).toURI());
		} catch (Exception e) {
			return null;
		}
	}

	@Test
	public void testJavaAPI1() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		dp.setDebugger(debugger);
		dp.setDebugging(true);
		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 2 << 3);
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
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
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

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");

		// Serialize the parser to memory, then deserialize for parsing.
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		WritableByteChannel output = Channels.newChannel(os);
		dp.save(output);

		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		ReadableByteChannel input = Channels.newChannel(is);
		edu.illinois.ncsa.daffodil.japi.Compiler compiler = Daffodil.compiler();
		DataProcessor parser = compiler.reload(input);
		parser.setDebugger(debugger);
		parser.setDebugging(true);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = parser.parse(rbc, outputter, 2 << 3);
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
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
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

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		dp.setDebugger(debugger);
		dp.setDebugging(true);

		// Serialize the parser to memory, then deserialize for parsing.
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		WritableByteChannel output = Channels.newChannel(os);
		dp.save(output);

		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		ReadableByteChannel input = Channels.newChannel(is);
		edu.illinois.ncsa.daffodil.japi.Compiler compiler = Daffodil.compiler();
		DataProcessor parser = compiler.reload(input);

		try {
			parser.setValidationMode(ValidationMode.Full);
			fail();
		} catch (InvalidUsageException e) {
			assertEquals("'Full' validation not allowed when using a restored parser.", e.getMessage());
		}

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
	}

	@Test
	public void testJavaAPI2() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Info);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myDataBroken.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);

		// TODO: NEED a java friendly way to get the status of the outputter. Scala enums don't work well
		//assertTrue(outputter.getStatus() != Status.DONE); // This is a hack so that Java doesn't have to know about Nope/Maybe, need to figure out better api that is Java compatible
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
		assertTrue(loc.toString().contains("mySchema2.dfdl.xsd"));

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
	public void testJavaAPI3() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema3.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		pf.setDistinguishedRootNode("e3", null);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData16.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 16 << 3);
		boolean err = res.isError();
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(2, res.location().bytePos1b());
		assertEquals(9, res.location().bitPos1b());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
		err = res2.isError();
		assertFalse(err);
		assertEquals("9", bos.toString());
	}

	// This is a duplicate of test testJavaAPI3 that serializes the parser
	// before executing the test.
	@Test
	public void testJavaAPI3_A() throws Exception {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema3.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		pf.setDistinguishedRootNode("e3", null);
		DataProcessor dp = pf.onPath("/");

		// Serialize the parser to memory, then deserialize for parsing.
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		WritableByteChannel output = Channels.newChannel(os);
		dp.save(output);

		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		ReadableByteChannel input = Channels.newChannel(is);
		edu.illinois.ncsa.daffodil.japi.Compiler compiler = Daffodil.compiler();
		DataProcessor parser = compiler.reload(input);

		java.io.File file = getResource("/test/japi/myData16.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = parser.parse(rbc, outputter, 16 << 3);
		boolean err = res.isError();
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(2, res.location().bytePos1b());
		assertEquals(9, res.location().bitPos1b());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
		err = res2.isError();
		assertFalse(err);
		assertEquals("9", bos.toString());
	}

	@Test
	public void testJavaAPI4b() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		File schemaFileName = getResource("/test/japi/mySchema3.dfdl.xsd");
		c.setDistinguishedRootNode("e4", null);
		ProcessorFactory pf = c.compileFile(schemaFileName);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData2.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 64 << 3);
		boolean err = res.isError();
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(5, res.location().bytePos1b());
		assertEquals(33, res.location().bitPos1b());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
		err = res2.isError();
		assertFalse(err);
		assertEquals("data", bos.toString());
	}

	@Test
	public void testJavaAPI5() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		File schemaFileName = getResource("/test/japi/mySchema3.dfdl.xsd");
		c.setDistinguishedRootNode("e4", null); // e4 is a 4-byte long string
												// element
		ProcessorFactory pf = c.compileFile(schemaFileName);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData3.dat"); // contains 5
																	// bytes
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 4 << 3);
		boolean err = res.isError();
		assertFalse(err);
		assertTrue("Assertion failed: End of data not reached.", res.location().isAtEnd());
		assertEquals(5, res.location().bytePos1b());
		assertEquals(33, res.location().bitPos1b());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
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

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
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
	public void testJavaAPI7() throws IOException {
		// TODO: This is due to the fact that we are doing several conversions
		// back and forth between Scala.xml.Node and JDOM. And the conversions
		// both use XMLOutputter to format the result (which escapes the
		// entities).
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel", null);
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);
		boolean err = res.isError();
		assertFalse(err);
		assertTrue(res.location().isAtEnd());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
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
	public void testJavaAPI8() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel2", null);
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);
		boolean err = res.isError();
		assertFalse(err);
		assertTrue(res.location().isAtEnd());

		java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc = java.nio.channels.Channels.newChannel(bos);
		UnparseResult res2 = dp.unparse(wbc, outputter.getResult());
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
	public void testJavaAPI9() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel2", null);
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);
		boolean err = res.isError();
		assertFalse(err);
		assertTrue(res.location().isAtEnd());

		org.jdom2.Document doc1 = outputter.getResult();

		java.io.ByteArrayOutputStream bos1 = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc1 = java.nio.channels.Channels.newChannel(bos1);
		UnparseResult res2 = dp.unparse(wbc1, doc1);
		err = res2.isError();
		assertFalse(err);
		assertTrue(bos1.toString().contains("Return-Path: <bob@smith.com>"));

		org.jdom2.Document doc2 = outputter.getResult();

		java.io.ByteArrayOutputStream bos2 = new java.io.ByteArrayOutputStream();
		java.nio.channels.WritableByteChannel wbc2 = java.nio.channels.Channels.newChannel(bos2);
		UnparseResult res3 = dp.unparse(wbc2, doc2);
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
	public void testJavaAPI10() throws IOException {

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema4.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData4.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);
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
	public void testJavaAPI11() throws IOException {

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema5.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData5.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter);
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
	public void testJavaAPI12() throws IOException {
		LogWriterForJAPITest2 lw2 = new LogWriterForJAPITest2();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw2);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);

		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		dp.setDebugger(debugger);
		dp.setDebugging(true);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 2 << 3);
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
	public void testJavaAPI13() throws IOException {
		// Demonstrates here that we can set external variables
		// after compilation but before parsing via Compiler.
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File extVarsFile = getResource("/test/japi/external_vars_1.xml");
		java.io.File schemaFile = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
		c.setExternalDFDLVariables(extVarsFile);
		ProcessorFactory pf = c.compileFile(schemaFile);

		DataProcessor dp = pf.onPath("/");
		dp.setDebugger(debugger);
		dp.setDebugging(true);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 2 << 3);
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
	public void testJavaAPI14() throws IOException {
		// Demonstrates here that we can set external variables
		// after compilation but before parsing via DataProcessor.
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File extVarFile = getResource("/test/japi/external_vars_1.xml");
		java.io.File schemaFile = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
		dp.setDebugger(debugger);
		dp.setDebugging(true);
		dp.setExternalVariables(extVarFile);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels.newChannel(fis);
		JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
		ParseResult res = dp.parse(rbc, outputter, 2 << 3);
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
	public void testJavaAPI15() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Info);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
		ProcessorFactory pf = c.compileFile(schemaFile);
		DataProcessor dp = pf.onPath("/");
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
		UnparseResult res = dp.unparse(wbc, doc);
		boolean err = res.isError();
		assertTrue(err);

		java.util.List<Diagnostic> diags = res.getDiagnostics();
		assertEquals(1, diags.size());
		Diagnostic d = diags.get(0);
		assertTrue(d.getMessage().contains("wrong"));
		assertTrue(d.getMessage().contains("e1"));

		// reset the global logging state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
	}


}
