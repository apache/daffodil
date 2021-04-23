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
 * <h2>Overview</h2>
 *
 * The {@link org.apache.daffodil.japi.Daffodil} object is a factory object to create a
 * {@link org.apache.daffodil.japi.Compiler}. The {@link org.apache.daffodil.japi.Compiler} provides
 * a method to compile a provided DFDL schema into a {@link org.apache.daffodil.japi.ProcessorFactory},
 * which creates a {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(file);
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor} provides the necessary functions to parse and
 * unparse data, returning a {@link org.apache.daffodil.japi.ParseResult} or
 * {@link org.apache.daffodil.japi.UnparseResult}, respectively. These contain information about the
 * parse/unparse, such as whether or not the processing succeeded with any diagnostic information.
 *
 * The {@link org.apache.daffodil.japi.DataProcessor} also provides two functions that can be used to
 * perform parsing/unparsing via the SAX API. The first creates a
 * {@link org.apache.daffodil.japi.DaffodilParseXMLReader} which is used for parsing, and the
 * second creates a {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler} which is used for
 * unparsing.
 *
 * <pre>
 * {@code
 * DaffodilParseXMLReader xmlReader = dp.newXMLReaderInstance();
 * DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(output);
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DaffodilParseXMLReader} has several methods that allow one to
 * set properties and handlers (such as ContentHandlers or ErrorHandlers) for the reader. One can
 * use any contentHandler/errorHandler as long as they extend the
 * {@link org.xml.sax.ContentHandler} and {@link org.xml.sax.ErrorHandler} interfaces
 * respectively. One can also set properties for the {@link org.apache.daffodil.japi.DaffodilParseXMLReader}
 * using {@link org.apache.daffodil.japi.DaffodilParseXMLReader#setProperty(java.lang.String,
 * java.lang.Object)}.
 *
 * The following properties can be set as follows:
 *
 * <p><i>The constants below have literal values starting with
 * "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:" and ending with "BlobDirectory",
 * "BlobPrefix" and "BlobSuffix" respectively.</i></p>
 *
 * <pre>
 * {@code
 * xmlReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY(),
 *  Paths.get(System.getProperty("java.io.tmpdir"))); // value type: java.nio.file.Paths
 * xmlReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX(), "daffodil-sax-"); // value type String
 * xmlReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX(), ".bin"); // value type String
 * }
 * </pre>
 *
 * The properties can be retrieved using the same variables with
 * {@link org.apache.daffodil.japi.DaffodilParseXMLReader#getProperty(java.lang.String)} and casting
 * to the appropriate type as listed above.
 *
 * The following handlers can be set as follows:
 * <pre>
 * {@code
 * xmlReader.setContentHandler(contentHandler);
 * xmlReader.setErrorHandler(errorHandler);
 * }
 * </pre>
 *
 * The handlers above must implement the following interfaces respectively:
 * <pre>
 * {@code
 * org.xml.sax.ContentHandler
 * org.xml.sax.ErrorHandler
 * }
 * </pre>
 *
 * The {@link org.apache.daffodil.japi.ParseResult} can be found as a property within the
 * {@link org.apache.daffodil.japi.DaffodilParseXMLReader} using this uri:
 * "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:ParseResult" or
 * {@link org.apache.daffodil.japi.DaffodilParseXMLReader#DAFFODIL_SAX_URN_PARSERESULT()}.
 *
 * In order for a successful unparse to happen, the SAX API requires the
 * unparse to be kicked off by a parse call to any {@link org.xml.sax.XMLReader} implementation that
 * has the {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler} registered as its content
 * handler. To retrieve the {@link org.apache.daffodil.japi.UnparseResult}, one can use
 * {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler#getUnparseResult()} once the
 * XMLReader.parse run is complete.
 *
 * <h3>Parse</h3>
 *
 * <h4>Dataprocessor Parse</h4>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(org.apache.daffodil.japi.io.InputSourceDataInputStream,
 * org.apache.daffodil.japi.infoset.InfosetOutputter)} method accepts input data to parse in the form
 * of a {@link org.apache.daffodil.japi.io.InputSourceDataInputStream} and an
 * {@link org.apache.daffodil.japi.infoset.InfosetOutputter} to determine the output representation
 * of the infoset (e.g. Scala XML Nodes, JDOM2 Documents, etc.):
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter= new JDOMInfosetOutputter();
 * InputSourceDataInputStream is = new InputSourceDataInputStream(data);
 * ParseResult pr = dp.parse(is, jdomOutputter);
 * Document doc = jdomOutputter.getResult();
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(org.apache.daffodil.japi.io.InputSourceDataInputStream,
 * org.apache.daffodil.japi.infoset.InfosetOutputter)} method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * {@link org.apache.daffodil.japi.infoset.InfosetOutputter}'s are not thread safe, requiring a
 * unique instance per thread. An {@link org.apache.daffodil.japi.infoset.InfosetOutputter} should
 * call {@link org.apache.daffodil.japi.infoset.InfosetOutputter#reset()} before reuse (or a new one
 * should be allocated). For example:
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
 * One can repeat calls to parse() using the same InputSourceDataInputStream to continue parsing
 * where the previous parse ended. For example:
 *
 * <pre>
 * {@code
 * InputSourceDataInputStream is = new InputSourceDataInputStream(dataStream);
 * JDOMInfosetOutputter jdomOutputter = new JDOMInfosetOutputter();
 * boolean keepParsing = true;
 * while (keepParsing && is.hasData()) {
 *   jdomOutputter.reset();
 *   ParseResult pr = dp.parse(is, jdomOutputter);
 *   ...
 *   keepParsing = !pr.isError();
 * }
 * }</pre>
 *
 * <h4>SAX Parse</h4>
 * The {@link org.apache.daffodil.japi.DaffodilParseXMLReader#parse(
 * org.apache.daffodil.japi.io.InputSourceDataInputStream)} method accepts input data to parse in
 * the form of a {@link org.apache.daffodil.japi.io.InputSourceDataInputStream}. The output
 * representation of the infoset, as well as how parse errors are handled, are dependent on the
 * content handler and the error handler provided to the {@link org.apache.daffodil.japi.DaffodilParseXMLReader}.
 * For example the {@link org.jdom2.input.sax.SAXHandler} provides a JDOM representation, whereas
 * other ContentHandlers may output directly to a {@link java.io.OutputStream} or {@link java.io.Writer}.
 *
 * <pre>
 * {@code
 * SAXHandler contentHandler = new SAXHandler();
 * xmlReader.setContentHandler(contentHandler);
 * InputSourceDataInputStream is = new InputSourceDataInputStream(data);
 * xmlReader.parse(is);
 * ParseResult pr = (ParseResult) xmlReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
 * Document doc = saxHandler.getDocument();
 * }</pre>
 *
 * The The {@link org.apache.daffodil.japi.DaffodilParseXMLReader#parse(
 * org.apache.daffodil.japi.io.InputSourceDataInputStream)} method is not thread-safe and may
 * only be called again/reused once a parse operation is completed. This can be done multiple
 * times without the need to create new DaffodilParseXMLReaders, ContentHandlers or ErrorHandlers.
 * It might be necessary to reset whatever ContentHandler is used (or allocate a new one). A
 * thread-safe implementation would require unique instances of the DaffodilParseXMLReader and its
 * components. For example:
 *
 * <pre>
 * {@code
 * SAXHandler contentHandler = new SAXHandler();
 * xmlReader.setContentHandler(contentHandler);
 * for (File f : inputFiles) {
 *   contentHandler.reset();
 *   InputSourceDataInputStream is = new InputSourceDataInputStream(new FileInputStream(f));
 *   xmlReader.parse(is);
 *   ParseResult pr = (ParseResult) xmlReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
 *   Document doc = saxHandler.getDocument();
 * }
 * }
 * </pre>
 *
 * The value of the supported features cannot be changed during a parse, and the parse will run
 * with the value of the features as they were when the parse was kicked off. To run a parse with
 * different feature values, one must wait until the running parse finishes, set the feature values
 * using the XMLReader's setFeature and run the parse again.
 *
 * One can repeat calls to parse() using the same InputSourceDataInputStream to continue parsing
 * where the previous parse ended. For example:
 *
 * <pre>
 * {@code
 * InputSourceDataInputStream is = new InputSourceDataInputStream(dataStream);
 * SAXHandler contentHandler = new SAXHandler();
 * xmlReader.setContentHandler(contentHandler);
 * Boolean keepParsing = true;
 * while (keepParsing && is.hasData()) {
 *   contentHandler.reset();
 *   xmlReader.parse(is);
 *   val pr = xmlReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
 *   ...
 *   keepParsing = !pr.isError();
 * }
 * }
 * </pre>
 *
 * <h3>Unparse</h3>
 *
 * <h4>Dataprocessor Unparse</h4>
 *
 * The same {@link org.apache.daffodil.japi.DataProcessor} used for parse can be used to unparse an
 * infoset via the {@link org.apache.daffodil.japi.DataProcessor#unparse(org.apache.daffodil.japi.infoset.InfosetInputter,
 * java.nio.channels.WritableByteChannel)} method. An {@link org.apache.daffodil.japi.infoset.InfosetInputter}
 * provides the infoset to unparse, with the unparsed data written to the
 * provided {@link java.nio.channels.WritableByteChannel}. For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetInputter jdomInputter = new JDOMInfosetInputter(doc);
 * UnparseResult ur = dp.unparse(jdomInputter, wbc)
 * }</pre>
 *
 * <h4>SAX Unparse</h4>
 *
 * In order to kick off an unparse via the SAX API, one must register the
 * {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler} as the contentHandler for an
 * XMLReader implementation. The call to the
 * {@link org.apache.daffodil.japi.DataProcessor#newContentHandlerInstance(java.nio.channels.WritableByteChannel)}
 * method must be provided with the {@link java.nio.channels.WritableByteChannel}, where the unparsed
 * data ought to be written to. Any XMLReader implementation is permissible, as long as they have
 * XML Namespace support.
 *
 * <pre>
 * {@code
 *  ByteArrayInputStream is = new ByteArrayInputStream(data);
 *  ByteArrayOutputStream os = new ByteArrayOutputStream();
 *  WritableByteChannel wbc = java.nio.channels.Channels.newChannel(os);
 *  DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(wbc);
 *  try {
 *   XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
 *   xmlReader.setContentHandler(unparseContentHandler)
 *   xmlReader.parse(is)
 *  } catch (ParserConfigurationException | SAXException e) {
 *   ...
 * `} catch catch (DaffodilUnparseErrorSAXException | DaffodilUnhandledSAXException e) {
 *   ...
 *  }
 * }
 * </pre>
 *
 * The call to the XMLReader.parse method must be wrapped in a try/catch, as
 * {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler} relies on throwing an exception to
 * end processing in the case of any errors/failures.
 * There are two kinds of errors to expect
 * {@link org.apache.daffodil.japi.DaffodilUnparseErrorSAXException}, for the case when the
 * {@link org.apache.daffodil.japi.UnparseResult#isError()}, and
 * {@link org.apache.daffodil.japi.DaffodilUnparseErrorSAXException}, for any other errors.
 *
 * In the case of an {@link org.apache.daffodil.japi.DaffodilUnhandledSAXException},
 * {@link org.apache.daffodil.japi.DaffodilUnparseContentHandler#getUnparseResult()} will return null.
 *
 * <pre>
 * {@code
 *  try {
 *    xmlReader.parse(new InputSource(is));
 *  } catch (DaffodilUnparseErrorSAXException | DaffodilUnhandledSAXException e) {
 *    ...
 *  }
 *  UnparseResult ur = unparseContentHandler.getUnparseResult();
 * }
 * </pre>
 *
 *
 * <h3>Failures and Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * {@link org.apache.daffodil.japi.ProcessorFactory}, {@link org.apache.daffodil.japi.DataProcessor},
 * or {@link org.apache.daffodil.japi.ParseResult}. However, rather than throwing an exception on
 * error (e.g. invalid DFDL schema, parse error, etc), these classes extend
 * {@link org.apache.daffodil.japi.WithDiagnostics}, which is used to determine if an error occurred,
 * and any diagnostic information (see {@link org.apache.daffodil.japi.Diagnostic}) related to the step.
 * Thus, before continuing, one must check {@link org.apache.daffodil.japi.WithDiagnostics#isError}.
 * For example:
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
 * DaffodilParseXMLReader xmlReader = dp.newXMLReaderInstance();
 * ... // setting appropriate handlers
 * xmlReader.parse(data);
 * ParseResult pr = xmlReader.getProperty("...ParseResult");
 * }</pre>
 *
 */

package org.apache.daffodil.japi;

