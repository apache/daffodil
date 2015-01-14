package edu.illinois.ncsa.daffodil.example;

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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
import edu.illinois.ncsa.daffodil.japi.LocationInSchemaFile;
import edu.illinois.ncsa.daffodil.japi.ParseResult;
import edu.illinois.ncsa.daffodil.japi.ProcessorFactory;
import edu.illinois.ncsa.daffodil.japi.logger.ConsoleLogWriter;
import edu.illinois.ncsa.daffodil.japi.logger.LogLevel;

public class TestJavaAPI {

	public java.io.File getResource(String resPath) {
		try {
			return new java.io.File(this.getClass().getResource(resPath)
					.toURI());
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
		Daffodil.setDebugger(debugger);
		Daffodil.setDebugging(true);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());

		for (String e : lw.errors)
			System.err.println(e);
		for (String e : lw.warnings)
			System.err.println(e);
		assertEquals(0, lw.errors.size());
		assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0); // got rid of include info messages
		// (too noisy)
		assertTrue(lw.others.size() > 0);
		assertTrue(debugger.lines.size() > 0);
		assertTrue(debugger.lines
				.contains("----------------------------------------------------------------- 1\n"));
		assertTrue(debugger.getCommand().equals("trace"));

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
		Daffodil.setDebugger(null);
		Daffodil.setDebugging(false);

	}

	// This is a duplicate of test testJavaAPI1 that serializes the parser
	// before executing the test.
	@Test
	public void testJavaAPI1_A() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);
		Daffodil.setDebugger(debugger);
		Daffodil.setDebugging(true);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");

		// Serialize the parser to memory, then deserialize for parsing.
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		WritableByteChannel output = Channels.newChannel(os);
		dp.save(output);

		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		ReadableByteChannel input = Channels.newChannel(is);
		edu.illinois.ncsa.daffodil.japi.Compiler compiler = Daffodil.compiler();
		DataProcessor parser = compiler.reload(input);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = parser.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());

		for (String e : lw.errors)
			System.err.println(e);
		for (String e : lw.warnings)
			System.err.println(e);
		assertEquals(0, lw.errors.size());
		assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0); // got rid of include info messages
		// (too noisy)
		assertTrue(lw.others.size() > 0);
		assertTrue(debugger.lines.size() > 0);
		assertTrue(debugger.lines
				.contains("----------------------------------------------------------------- 1\n"));
		assertTrue(debugger.getCommand().equals("trace"));

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
		Daffodil.setDebugger(null);
		Daffodil.setDebugging(false);
	}

	@Test
	public void testJavaAPI2() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Info);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myDataBroken.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		try {
			@SuppressWarnings("unused")
			org.jdom2.Document doc = res.result();
			fail("did not throw");
		} catch (Exception e) {
			assertTrue(e.getMessage().contains("no result"));
		}
		assertTrue(res.isError());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		assertEquals(1, diags.size());
		Diagnostic d = diags.get(0);
		System.err.println(d.getMessage());
		assertTrue(d.getMessage().contains("int"));
		assertTrue(d.getMessage().contains("Not an int"));
		assertTrue(d.getDataLocations().toString().contains("10"));
		java.util.List<LocationInSchemaFile> locs = d
				.getLocationsInSchemaFiles();
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
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		pf.setDistinguishedRootNode("e3", null);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData16.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 16 << 3);
		boolean err = res.isError();
		org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom2.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(2, res.location().bytePos1b());
		assertEquals(9, res.location().bitPos1b());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());
	}

	// This is a duplicate of test testJavaAPI3 that serializes the parser
	// before executing the test.
	@Test
	public void testJavaAPI3_A() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
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
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = parser.parse(rbc, 16 << 3);
		boolean err = res.isError();
		org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom2.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(2, res.location().bytePos1b());
		assertEquals(9, res.location().bitPos1b());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());
	}

	/*
	 * @Test public void testJavaAPI4() throws IOException { Compiler c =
	 * Daffodil.compiler(); String[] schemaFileNames = new String[1];
	 * schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
	 * ProcessorFactory pf = c.compileFiles(schemaFileNames);
	 * pf.setDistinguishedRootNode("e4", null); DataProcessor dp =
	 * pf.onPath("/"); java.io.File file = new
	 * java.io.File(getResource("/test/japi/myData2.dat"));
	 * java.io.FileInputStream fis = new java.io.FileInputStream(file);
	 * java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
	 * .newChannel(fis); ParseResult res = dp.parse(rbc, 64 << 3); boolean err =
	 * res.isError(); org.jdom2.output.XMLOutputter xo = new
	 * org.jdom2.output.XMLOutputter(); xo.setFormat(Format.getPrettyFormat());
	 * java.util.List<Diagnostic> diags = res.getDiagnostics(); for (Diagnostic
	 * d : diags) { System.err.println(d.getMessage()); } if (!err) {
	 * org.jdom2.Document doc = res.result(); xo.output(doc, System.out); }
	 * assertFalse(err); assertFalse(res.location().isAtEnd()); assertEquals(4,
	 * res.location().bytePos()); assertEquals(32, res.location().bitPos());
	 * System.err.println("bitPos = " + res.location().bitPos());
	 * System.err.println("bytePos = " + res.location().bytePos()); }
	 */

	@Test
	public void testJavaAPI4b() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		File[] schemaFileNames = new File[1];
		schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		c.setDistinguishedRootNode("e4", null);
		ProcessorFactory pf = c.compileFiles(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData2.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 64 << 3);
		boolean err = res.isError();
		org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom2.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(5, res.location().bytePos1b());
		assertEquals(33, res.location().bitPos1b());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());
	}

	@Test
	public void testJavaAPI5() throws IOException {
		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		File[] schemaFileNames = new File[1];
		schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		c.setDistinguishedRootNode("e4", null); // e4 is a 4-byte long string
												// element
		ProcessorFactory pf = c.compileFiles(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData3.dat"); // contains 5
																	// bytes
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 4 << 3);
		boolean err = res.isError();
		org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom2.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertTrue("Assertion failed: End of data not reached.", res.location()
				.isAtEnd());
		assertEquals(5, res.location().bytePos1b());
		assertEquals(33, res.location().bitPos1b());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());
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
		java.io.File[] schemaFiles = new java.io.File[4];
		// String[] schemaFileNames = new String[2];
		schemaFiles[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFiles[1] = new java.io.File("/test/japi/notHere1.dfdl.xsd");
		schemaFiles[2] = getResource("/test/japi/mySchema2.dfdl.xsd");
		schemaFiles[3] = new java.io.File("/test/japi/notHere2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		assertTrue(pf.isError());
		List<Diagnostic> diags = pf.getDiagnostics();
		boolean found1 = false;
		boolean found2 = false;
		for (Diagnostic d : diags) {
			if (d.getMessage().contains("notHere1")) {
				found1 = true;
			}
			if (d.getMessage().contains("notHere2")) {
				found2 = true;
			}
		}
		assertTrue(found1);
		assertTrue(found2);

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
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel", null);
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			// xo.setFormat(Format.getPrettyFormat());
			xo.setFormat(Format.getRawFormat().setTextMode(
					Format.TextMode.PRESERVE));
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());

		// assertEquals(0, lw.errors.size());
		// assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0);
		// assertTrue(lw.others.size() > 0);

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
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel2", null);
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			// xo.setFormat(Format.getPrettyFormat());
			xo.setFormat(Format.getRawFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());

		// assertEquals(0, lw.errors.size());
		// assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0);
		// assertTrue(lw.others.size() > 0);

		// reset the global logging state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
	}

	/**
	 * Verify that calling result() on the ParseResult mutiple times does not
	 * error.
	 */
	@Test
	public void testJavaAPI9() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/TopLevel.xsd");
		c.setDistinguishedRootNode("TopLevel2", null);
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/01very_simple.txt");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			// org.jdom2.Document doc2 = res.result();
			// org.jdom2.Document doc3 = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getRawFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());

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
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/mySchema4.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData4.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
			org.jdom2.Element rootNode = doc.getRootElement();
			org.jdom2.Element hidden = rootNode.getChild("hiddenElement",
					rootNode.getNamespace());
			assertTrue(null == hidden);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
	}

	/**
	 * Verify that nested elements do not appear as duplicates
	 */
	@Test
	public void testJavaAPI11() throws IOException {

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File[] schemaFiles = new java.io.File[1];
		schemaFiles[0] = getResource("/test/japi/mySchema5.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData5.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
			org.jdom2.Element rootNode = doc.getRootElement();
			org.jdom2.Element elementGroup = rootNode.getChild("elementGroup",
					null); // local element names are unqualified
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
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
	}

	@Test
	public void testJavaAPI12() throws IOException {
		LogWriterForJAPITest2 lw2 = new LogWriterForJAPITest2();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw2);
		Daffodil.setLoggingLevel(LogLevel.Debug);
		Daffodil.setDebugger(debugger);
		Daffodil.setDebugging(true);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);

		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());

		for (String e : lw2.errors)
			System.err.println(e);
		for (String e : lw2.warnings)
			System.err.println(e);
		assertEquals(0, lw2.errors.size());
		assertEquals(0, lw2.warnings.size());
		// assertTrue(lw2.infos.size() > 0);
		assertTrue(lw2.others.size() > 0);
		assertTrue(debugger.lines.size() > 0);
		assertTrue(debugger.lines
				.contains("----------------------------------------------------------------- 1\n"));

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
		Daffodil.setDebugger(null);
		Daffodil.setDebugging(false);
	}

	@Test
	public void testJavaAPI13() throws IOException {
		// Demonstrates here that we can set external variables
		// after compilation but before parsing via Compiler.
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);
		Daffodil.setDebugger(debugger);
		Daffodil.setDebugging(true);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File extVarsFile = getResource("/test/japi/external_vars_1.xml");
		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		c.setExternalDFDLVariables(extVarsFile);
		ProcessorFactory pf = c.compileFiles(schemaFiles);

		DataProcessor dp = pf.onPath("/");

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
			String docString = xo.outputString(doc);
			boolean containsVar1 = docString.contains("var1Value");
			boolean containsVar1Value = docString.contains("externallySet");
			assertTrue(containsVar1);
			assertTrue(containsVar1Value);
		}
		// java.util.List<Diagnostic> diags = res.getDiagnostics();
		// for (Diagnostic d : diags) {
		// System.err.println(d.getMessage());
		// }
		// assertTrue(res.location().isAtEnd());
		// System.err.println("bitPos = " + res.location().bitPos());
		// System.err.println("bytePos = " + res.location().bytePos());
		//
		// for (String e : lw.errors)
		// System.err.println(e);
		// for (String e : lw.warnings)
		// System.err.println(e);
		// assertEquals(0, lw.errors.size());
		// assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0);
		// assertTrue(lw.others.size() > 0);
		// assertTrue(debugger.lines.size() > 0);
		// assertTrue(debugger.lines.contains("----------------------------------------------------------------- 1\n"));

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
		Daffodil.setDebugger(null);
		Daffodil.setDebugging(false);
	}

	@Test
	public void testJavaAPI14() throws IOException {
		// Demonstrates here that we can set external variables
		// after compilation but before parsing via DataProcessor.
		LogWriterForJAPITest lw = new LogWriterForJAPITest();
		DebuggerRunnerForJAPITest debugger = new DebuggerRunnerForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);
		Daffodil.setDebugger(debugger);
		Daffodil.setDebugging(true);

		edu.illinois.ncsa.daffodil.japi.Compiler c = Daffodil.compiler();
		c.setValidateDFDLSchemas(false);
		java.io.File extVarFile = getResource("/test/japi/external_vars_1.xml");
		java.io.File[] schemaFiles = new java.io.File[2];
		schemaFiles[0] = getResource("/test/japi/mySchemaWithVars.dfdl.xsd");
		schemaFiles[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compileFiles(schemaFiles);
		DataProcessor dp = pf.onPath("/");
		dp.setExternalVariables(extVarFile);

		java.io.File file = getResource("/test/japi/myData.dat");
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom2.Document doc = res.result();
			org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
			String docString = xo.outputString(doc);
			boolean containsVar1 = docString.contains("var1Value");
			boolean containsVar1Value = docString.contains("externallySet");
			assertTrue(containsVar1);
			assertTrue(containsVar1Value);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
		System.err.println("bitPos = " + res.location().bitPos1b());
		System.err.println("bytePos = " + res.location().bytePos1b());

		for (String e : lw.errors)
			System.err.println(e);
		for (String e : lw.warnings)
			System.err.println(e);
		assertEquals(0, lw.errors.size());
		assertEquals(0, lw.warnings.size());
		// assertTrue(lw.infos.size() > 0);
		assertTrue(lw.others.size() > 0);
		assertTrue(debugger.lines.size() > 0);
		assertTrue(debugger.lines
				.contains("----------------------------------------------------------------- 1\n"));

		// reset the global logging and debugger state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
		Daffodil.setDebugger(null);
		Daffodil.setDebugging(false);
	}

}
