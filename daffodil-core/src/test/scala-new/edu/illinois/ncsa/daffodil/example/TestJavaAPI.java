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
		ParseResult res = dp.parse(rbc);
		org.jdom.Document doc = res.result();
		org.jdom.output.XMLOutputter xo = new org.jdom.output.XMLOutputter();
		xo.setFormat(Format.getPrettyFormat());
		xo.output(doc, System.out);
		java.util.List<Diagnostic> diags = res.getDiagnostics();
		for (Diagnostic d : diags) {
			System.err.println(d.getMessage());
		}
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
		} catch (IllegalStateException e) {
			// we're good
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
}
