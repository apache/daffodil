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

/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The {@link org.apache.daffodil.japi.Daffodil} object is a factory object to create a {@link org.apache.daffodil.japi.Compiler}. The
 * {@link org.apache.daffodil.japi.Compiler} provides a method to compile a provided DFDL schema into a
 * {@link org.apache.daffodil.japi.ProcessorFactory}, which creates a {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(file);
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor} provides the necessary functions to parse and unparse
 * data, returning a {@link org.apache.daffodil.japi.ParseResult} or {@link org.apache.daffodil.japi.UnparseResult}, respectively. These
 * contain information about the parse/unparse, such as whether or not the
 * processing succeeded with any diagnostic information.
 *
 * The {@link org.apache.daffodil.japi.DataProcessor} also provides a function to create a
 * {@link org.apache.daffodil.japi.DaffodilXMLReader} that can be used to perform parsing via the
 * SAX API.
 *
 * <pre>
 * {@code
 * DaffodilXMLReader xmlRdr = dp.newXMLReaderInstance();
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DaffodilXMLReader} has several methods that allow one to
 * set properties and handlers (such as ContentHandlers or ErrorHandlers) for the reader. One can
 * use any contentHandler/errorHandler as long as they extend the
 * {@link org.xml.sax.ContentHandler} and {@link org.xml.sax.ErrorHandler} interfaces
 * respectively. One can also set properties for the {@link org.apache.daffodil.japi.DaffodilXMLReader}
 * using {@link org.apache.daffodil.japi.DaffodilXMLReader#setProperty(java.lang.String, java.lang.Object)}.
 *
 * The following properties can be set as follows:
 * <pre>
 * {@code
 * xmlRdr.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY(), "/tmp/");
 * xmlRdr.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX(), "daffodil-sax-");
 * xmlRdr.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX(), ".bin");
 * }
 * </pre>
 *
 * The variables above start with "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:" and end
 * with BlobDirectory, BlobPrefix and BlobSuffix respectively.
 *
 * The properites can be retrieved using the same variables with
 * {@link org.apache.daffodil.japi.DaffodilXMLReader#getProperty(java.lang.String)}
 *
 * The following handlers can be set as follows:
 * <pre>
 * {@code
 * xmlRdr.setContentHandler(contentHandler);
 * xmlRdr.setErrorHandler(errorHandler);
 * xmlRdr.setDTDHandler(dtdHandler);
 * xmlRdr.setEntityResolver(entityResolver);
 * }
 * </pre>
 *
 * The handlers above must implement the following interfaces respectively:
 * <pre>
 * {@code
 * org.xml.sax.ContentHandler
 * org.xml.sax.ErrorHandler
 * org.xml.sax.DTDHandler
 * org.xml.sax.EntityResolver
 * }
 * </pre>
 *
 * The {@link org.apache.daffodil.japi.ParseResult} can be found as a property within the
 * {@link org.apache.daffodil.japi.DaffodilXMLReader} using
 * {@link org.apache.daffodil.japi.DaffodilXMLReader#getProperty(java.lang.String)} together
 * with the following uri: "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:ParseResult" or
 * XMLUtils.DAFFODIL_SAX_URN_PARSERESULT().
 *
 * <h4>Parse</h4>
 *
 * <h5>Dataprocessor Parse</h5>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(org.apache.daffodil.japi.io.InputSourceDataInputStream, org.apache.daffodil.japi.infoset.InfosetOutputter)} method accepts input data to parse in the form
 * of a {@link org.apache.daffodil.japi.io.InputSourceDataInputStream} and an {@link org.apache.daffodil.japi.infoset.InfosetOutputter}
 * to determine the output representation of the infoset (e.g. Scala XML Nodes,
 * JDOM2 Documents, etc.):
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter= new JDOMInfosetOutputter();
 * InputSourceDataInputStream is = new InputSourceDataInputStream(data);
 * ParseResult pr = dp.parse(is, jdomOutputter);
 * Document doc = jdomOutputter.getResult();
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(org.apache.daffodil.japi.io.InputSourceDataInputStream, org.apache.daffodil.japi.infoset.InfosetOutputter)} method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * {@link org.apache.daffodil.japi.infoset.InfosetOutputter}'s are not thread safe, requiring a unique instance per
 * thread. An {@link org.apache.daffodil.japi.infoset.InfosetOutputter} should call {@link org.apache.daffodil.japi.infoset.InfosetOutputter#reset()} before
 * reuse (or a new one should be allocated). For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter = new JDOMInfosetOutputter();
 * for (File f : inputFiles) {
 *   jdomOutputter.reset();
 *   InputSourceDataInputStream is = new InputSourceDataInputStream(new FileInputStream(f)));
 *   ParseResult pr = dp.parse(is, jdomOutputter);
 *   Document doc = jdomOutputter.getResult();
 * }
 * }</pre>
 *
 * One can repeat calls to parse() using the same InputSourceDataInputStream to continue parsing where the previous parse ended. For example:
 *
 * <pre>
 * {@code
 * InputSourceDataInputStream is = new InputSourceDataInputStream(dataStream);
 * JDOMInfosetOutputter jdomOutputter = new JDOMInfosetOutputter();
 * boolean keepParsing = true;
 * while (keepParsing) {
 *   jdomOutputter.reset();
 *   ParseResult pr = dp.parse(is, jdomOutputter);
 *   ...
 *   keepParsing = !pr.location().isAtEnd() && !pr.isError();
 * }
 * }</pre>
 *
 * <h5>SAX Parse</h5>
 * The {@link org.apache.daffodil.japi.DaffodilXMLReader#parse(
 * org.apache.daffodil.japi.io.InputSourceDataInputStream)} method accepts input data to parse in
 * the form of a {@link org.apache.daffodil.japi.io.InputSourceDataInputStream}. The output
 * representation of the infoset, as well as how parse errors are handled, are dependent on the
 * content handler and the error handler provided to the {@link org.apache.daffodil.japi.DaffodilXMLReader}. For example the
 * {@link org.jdom2.input.sax.SAXHandler} provides a JDOM representation, whereas other Content
 * Handlers may output directly to an {@link java.io.OutputStream} or {@link java.io.Writer}.
 *
 * <pre>
 * {@code
 * SAXHandler contentHandler = new SAXHandler();
 * xmlRdr.setContentHandler(contentHandler);
 * InputSourceDataInputStream is = new InputSourceDataInputStream(data);
 * xmlReader.parse(is);
 * ParseResult pr = (ParseResult) xmlRdr.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT());
 * Document doc = saxHandler.getDocument();
 * }</pre>
 *
 * The The {@link org.apache.daffodil.japi.DaffodilXMLReader#parse(
 * org.apache.daffodil.japi.io.InputSourceDataInputStream)} method is not thread-safe and may
 * only be called again/reused once a parse operation is completed. This can be done multiple
 * times without the need to create new DaffodilXMLReaders, ContentHandlers or ErrorHandlers. It
 * might be necessary to reset whatever ContentHandler is used (or allocate a new one). A
 * thread-safe implementation would require unique instances of the DaffodilXMLReader and its
 * components. For example:
 *
 * <pre>
 * {@code
 * SAXHandler contentHandler = new SAXHandler();
 * xmlRdr.setContentHandler(contentHandler);
 * for (File f : inputFiles) {
 *   contentHandler.reset();
 *   InputSourceDataInputStream is = new InputSourceDataInputStream(new FileInputStream(f));
 *   xmlReader.parse(is);
 *   ParseResult pr = (ParseResult) xmlRdr.getProperty("urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:ParseResult");
 *   Document doc = saxHandler.getDocument();
 * }
 * }
 * </pre>
 *
 * One can repeat calls to parse() using the same InputSourceDataInputStream to continue parsing
 * where the previous parse ended. For example:
 *
 * <pre>
 * {@code
 * InputSourceDataInputStream is = new InputSourceDataInputStream(dataStream);
 * SAXHandler contentHandler = new SAXHandler();
 * xmlRdr.setContentHandler(contentHandler);
 * Boolean keepParsing = true;
 * while (keepParsing) {
 *   contentHandler.reset();
 *   xmlRdr.parse(is);
 *   val pr = xmlRdr.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT());
 *   ...
 *   keepParsing = !pr.location().isAtEnd() && !pr.isError();
 * }
 * }
 * </pre>
 *
 * <h4>Unparse</h4>
 *
 * The same {@link org.apache.daffodil.japi.DataProcessor} used for parse can be used to unparse an infoset
 * via the {@link org.apache.daffodil.japi.DataProcessor#unparse(org.apache.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method. An {@link org.apache.daffodil.japi.infoset.InfosetInputter}
 * provides the infoset to unparse, with the unparsed data written to the
 * provided {@link java.nio.channels.WritableByteChannel}. For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetInputter jdomInputter = new JDOMInfosetInputter(doc);
 * UnparseResult ur = dp.unparse(jdomInputter, wbc)
 * }</pre>
 *
 * <h3>Failures and Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * {@link org.apache.daffodil.japi.ProcessorFactory}, {@link org.apache.daffodil.japi.DataProcessor}, or {@link org.apache.daffodil.japi.ParseResult}. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend {@link org.apache.daffodil.japi.WithDiagnostics}, which is used to
 * determine if an error occurred, and any diagnostic information (see
 * {@link org.apache.daffodil.japi.Diagnostic}) related to the step. Thus, before continuing, one must check
 * {@link org.apache.daffodil.japi.WithDiagnostics#isError}. For example:
 *
 * <pre>
 * {@code
 * ProcessorFactor pf = c.compile(files);
 * if (pf.isError()) {
 *   java.util.List<Diagnostic> diags = pf.getDiagnostics();
 *   foreach (Diagnostic d : diags) {
 *     System.out.println(d.toString());
 *   }
 *   return -1;
 * }
 * }</pre>
 *
 * <h3>Saving and Reloading Parsers</h3>
 *
 * In some cases, it may be beneficial to save a parser and reload it.
 * For example, when starting up, it may be quicker to reload an
 * already compiled parser than to compile it from scratch. To save a
 * {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = pf.onPath("/");
 * dp.save(saveFile);
 * }</pre>
 *
 * And to restore a saved {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = Daffodil.reload(saveFile);
 * ParseResult pr = dp.parse(data);
 * }</pre>
 *
 * And use like below:
 * <pre>
 * {@code
 * ParseResult pr = dp.parse(data);
 * }</pre>
 *
 * or
 *
 * <pre>
 * {@code
 * DaffodilXMLReader xmlRdr = dp.newXMLReaderInstance();
 * ... // setting appropriate handlers
 * xmlReader.parse(data);
 * ParseResult pr = xmlRdr.getProperty("...ParseResult");
 * }</pre>
 *
 */

package org.apache.daffodil.japi;

