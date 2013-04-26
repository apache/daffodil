package edu.illinois.ncsa.daffodil.example;

import static org.junit.Assert.*;

import java.io.IOException;

import edu.illinois.ncsa.daffodil.japi.*;
import edu.illinois.ncsa.daffodil.japi.Compiler;

import org.jdom.output.Format;
import org.junit.Test;

public class TestJavaAPI {

	@Test
	public void testJavaAPI1() throws IOException {
		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[2];
		schemaFileNames[0] = "src/test/resources/test/japi/mySchema1.dfdl.xsd";
		schemaFileNames[1] = "src/test/resources/test/japi/mySchema2.dfdl.xsd";
		ProcessorFactory pf = c.compile(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(
				"src/test/resources/test/japi/myData.dat");
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
	}

	@Test
	public void testJavaAPI2() throws IOException {
		Compiler c = Daffodil.compiler();
		String[] schemaFileNames = new String[2];
		schemaFileNames[0] = "src/test/resources/test/japi/mySchema1.dfdl.xsd";
		schemaFileNames[1] = "src/test/resources/test/japi/mySchema2.dfdl.xsd";
		ProcessorFactory pf = c.compile(schemaFileNames);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(
				"src/test/resources/test/japi/myDataBroken.dat");
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
		schemaFileNames[0] = "src/test/resources/test/japi/mySchema3.dfdl.xsd";
		ProcessorFactory pf = c.compile(schemaFileNames);
		pf.setDistinguishedRootNode("e3", null);
		DataProcessor dp = pf.onPath("/");
		java.io.File file = new java.io.File(
				"src/test/resources/test/japi/myData16.dat");
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
}
