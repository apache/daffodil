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

import static org.junit.Assert.*;

import java.io.IOException;

import edu.illinois.ncsa.daffodil.japi.*;
import edu.illinois.ncsa.daffodil.japi.Compiler;

import org.jdom.output.Format;
import org.junit.Test;

public class TestJavaAPI {

	public String getResource(String resPath) {
		try {
			return this.getClass().getResource(resPath).getPath();
		} catch (Exception e) {
			return "";
		}
	}

	@Test
	public void testJavaAPI1() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Debug);

		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[2];
		schemaFileNames[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFileNames[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compile(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(getResource("/test/japi/myData.dat"));
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 2 << 3);
		boolean err = res.isError();
		if (!err) {
			org.jdom.Document doc = res.result();
			org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
		}
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		assertTrue(res.location().isAtEnd());
		System.err.println("bitPos = " + res.location().bitPos());
		System.err.println("bytePos = " + res.location().bytePos());
	
		assertEquals(0, lw.errors.size());
		assertEquals(0, lw.warnings.size());
		assertTrue(lw.infos.size() > 0);
		assertTrue(lw.others.size() > 0);

		// reset the global logging state
		Daffodil.setLogWriter(new ConsoleLogWriter());
		Daffodil.setLoggingLevel(LogLevel.Info);
	}

	@Test
	public void testJavaAPI2() throws IOException {
		LogWriterForJAPITest lw = new LogWriterForJAPITest();

		Daffodil.setLogWriter(lw);
		Daffodil.setLoggingLevel(LogLevel.Info);

		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[2];
		schemaFileNames[0] = getResource("/test/japi/mySchema1.dfdl.xsd");
		schemaFileNames[1] = getResource("/test/japi/mySchema2.dfdl.xsd");
		ProcessorFactory pf = c.compile(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(getResource("/test/japi/myDataBroken.dat"));
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc);
		try {
			org.jdom.Document doc = res.result();
			fail("did not throw");
			org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
			xo.setFormat(Format.getPrettyFormat());
			xo.output(doc, System.out);
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
		assertTrue(lw.infos.size() > 0);
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
		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[1];
		schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		ProcessorFactory pf = c.compile(schemaFileNames);
		pf.setDistinguishedRootNode("e3", null);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(getResource("/test/japi/myData16.dat"));
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 16 << 3);
		boolean err = res.isError();
		org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(1, res.location().bytePos());
		assertEquals(8, res.location().bitPos());
		System.err.println("bitPos = " + res.location().bitPos());
		System.err.println("bytePos = " + res.location().bytePos());
	}
/*	
  @Test
	public void testJavaAPI4() throws IOException {
		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[1];
		schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
    ProcessorFactory pf = c.compile(schemaFileNames);
		pf.setDistinguishedRootNode("e4", null);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(getResource("/test/japi/myData2.dat"));
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 64 << 3);
		boolean err = res.isError();
		org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(4, res.location().bytePos());
		assertEquals(32, res.location().bitPos());
		System.err.println("bitPos = " + res.location().bitPos());
		System.err.println("bytePos = " + res.location().bytePos());
	}
  */
  
  @Test
	public void testJavaAPI4b() throws IOException {
		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[1];
		schemaFileNames[0] = getResource("/test/japi/mySchema3.dfdl.xsd");
		c.setDistinguishedRootNode("e4", null);
    ProcessorFactory pf = c.compile(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(getResource("/test/japi/myData2.dat"));
		java.io.FileInputStream fis = new java.io.FileInputStream(file);
		java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
				.newChannel(fis);
		ParseResult res = dp.parse(rbc, 64 << 3);
		boolean err = res.isError();
		org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
		if (!err) {
			org.jdom.Document doc = res.result();
			xo.output(doc, System.out);
		}
		assertFalse(err);
		assertFalse(res.location().isAtEnd());
		assertEquals(4, res.location().bytePos());
		assertEquals(32, res.location().bitPos());
		System.err.println("bitPos = " + res.location().bitPos());
		System.err.println("bytePos = " + res.location().bytePos());
	}
}
