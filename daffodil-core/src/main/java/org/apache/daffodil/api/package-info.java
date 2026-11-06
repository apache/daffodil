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
 * The {@link org.apache.daffodil.api.Daffodil} object is a factory object to create a
 * {@link org.apache.daffodil.api.Compiler}. The {@link org.apache.daffodil.api.Compiler} provides
 * a method to compile a provided DFDL schema into a {@link org.apache.daffodil.api.ProcessorFactory},
 * which creates a {@link org.apache.daffodil.api.DataProcessor}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(file);
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * The {@link org.apache.daffodil.api.DataProcessor} provides the necessary functions to parse and
 * unparse data, returning a {@link org.apache.daffodil.api.ParseResult} or
 * {@link org.apache.daffodil.api.UnparseResult}, respectively. These contain information about the
 * parse/unparse, such as whether or not the processing succeeded with any diagnostic information.
 *
 * <p>Compiling a DFDL schema is expensive and the compiled objects are large, so they are designed
 * to be created once and shared. See <b>Thread Safety</b>, at the end of this document, for which
 * of these objects may be shared by multiple threads and which must be unique to each thread.</p>
 *
 * The {@link org.apache.daffodil.api.DataProcessor} also provides two functions that can be used to
 * perform parsing/unparsing via the SAX API. The first creates a
 * {@link org.apache.daffodil.api.DaffodilParseXMLReader} which is used for parsing, and the
 * second creates a {@link org.apache.daffodil.api.DaffodilUnparseContentHandler} which is used for
 * unparsing.
 *
 * <pre>
 * {@code
 * DaffodilParseXMLReader xmlReader = dp.newXMLReaderInstance();
 * DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(output);
 * }</pre>
 *
 * The {@link org.apache.daffodil.api.DaffodilParseXMLReader} has several methods that allow one to
 * set properties and handlers (such as ContentHandlers or ErrorHandlers) for the reader. One can
 * use any contentHandler/errorHandler as long as they extend the
 * {@link org.xml.sax.ContentHandler} and {@link org.xml.sax.ErrorHandler} interfaces
 * respectively. One can also set properties for the {@link org.apache.daffodil.api.DaffodilParseXMLReader}
 * using {@link org.apache.daffodil.api.DaffodilParseXMLReader#setProperty(java.lang.String,
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
 * {@link org.apache.daffodil.api.DaffodilParseXMLReader#getProperty(java.lang.String)} and casting
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
 * The {@link org.apache.daffodil.api.ParseResult} can be found as a property within the
 * {@link org.apache.daffodil.api.DaffodilParseXMLReader} using this uri:
 * "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:sax:ParseResult" or
 * {@link org.apache.daffodil.api.DaffodilParseXMLReader#DAFFODIL_SAX_URN_PARSERESULT}.
 *
 * In order for a successful unparse to happen, the SAX API requires the
 * unparse to be kicked off by a parse call to any {@link org.xml.sax.XMLReader} implementation that
 * has the {@link org.apache.daffodil.api.DaffodilUnparseContentHandler} registered as its content
 * handler. To retrieve the {@link org.apache.daffodil.api.UnparseResult}, one can use
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler#getUnparseResult()} once the
 * XMLReader.parse run is complete.
 *
 * <h3>Parse</h3>
 *
 * <h4>DataProcessor Parse</h4>
 *
 * The {@link org.apache.daffodil.api.DataProcessor#parse(org.apache.daffodil.api.InputSourceDataInputStream,
 * org.apache.daffodil.api.infoset.InfosetOutputter)} method accepts input data to parse in the form
 * of a {@link org.apache.daffodil.api.InputSourceDataInputStream} and an
 * {@link org.apache.daffodil.api.infoset.InfosetOutputter} to determine the output representation
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
 * The {@link org.apache.daffodil.api.DataProcessor#parse(org.apache.daffodil.api.InputSourceDataInputStream,
 * org.apache.daffodil.api.infoset.InfosetOutputter)} method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * {@link org.apache.daffodil.api.infoset.InfosetOutputter}'s are not thread safe, requiring a
 * unique instance per thread. An {@link org.apache.daffodil.api.infoset.InfosetOutputter} should
 * call {@link org.apache.daffodil.api.infoset.InfosetOutputter#reset()} before reuse (or a new one
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
 * The {@link org.apache.daffodil.api.DaffodilParseXMLReader#parse(
 * org.apache.daffodil.api.InputSourceDataInputStream)} method accepts input data to parse in
 * the form of a {@link org.apache.daffodil.api.InputSourceDataInputStream}. The output
 * representation of the infoset, as well as how parse errors are handled, are dependent on the
 * content handler and the error handler provided to the {@link org.apache.daffodil.api.DaffodilParseXMLReader}.
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
 * The The {@link org.apache.daffodil.api.DaffodilParseXMLReader#parse(
 * org.apache.daffodil.api.InputSourceDataInputStream)} method is not thread-safe and may
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
 *   ParseResult pr = xmlReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
 *   ...
 *   keepParsing = !pr.isError();
 * }
 * }
 * </pre>
 *
 * <h3>Unparse</h3>
 *
 * <h4>DataProcessor Unparse</h4>
 *
 * The same {@link org.apache.daffodil.api.DataProcessor} used for parse can be used to unparse an
 * infoset via the {@link org.apache.daffodil.api.DataProcessor#unparse(org.apache.daffodil.api.infoset.InfosetInputter,
 * java.nio.channels.WritableByteChannel)} method. An {@link org.apache.daffodil.api.infoset.InfosetInputter}
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
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler} as the ContentHandler for an
 * XMLReader implementation. The call to the
 * {@link org.apache.daffodil.api.DataProcessor#newContentHandlerInstance(java.nio.channels.WritableByteChannel)}
 * method must be provided with the {@link java.nio.channels.WritableByteChannel}, where the unparsed
 * data ought to be written to. Any XMLReader implementation is permissible, as long as they have
 * XML Namespace support.
 *
 * The call to the XMLReader.parse method must be wrapped in a try/catch, as the
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler} relies on throwing an exception to
 * end processing in the case of any errors/failures. There are two kinds of exceptions it could throw:
 * <ul>
 * <li>{@link org.apache.daffodil.api.exceptions.DaffodilUnparseErrorSAXException} - thrown when a
 * processing error is encountered while unparsing. In this case,
 * {@link org.apache.daffodil.api.UnparseResult#isError()} is true.
 * </li>
 * <li>{@link org.apache.daffodil.api.exceptions.DaffodilUnhandledSAXException} - usually indicates
 * a bug in Daffodil. This is an unchecked RuntimeException and should usually not be explicitly
 * caught--it should be handled like one would handle any other RuntimeException. Note that if this
 * is thrown
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler#getUnparseResult()} returns null.
 * </li>
 * </ul>
 *
 * After the XMLReader parse has completed, either successfully or via a thrown exception, the
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler#finish()} method should be called--this
 * allows content handler state to be cleaned up and ensures the
 * {@link org.apache.daffodil.api.DaffodilUnparseContentHandler#getUnparseResult()} returns an
 * {@link org.apache.daffodil.api.UnparseResult} even in cases where the XMLReader encounters an
 * error. For example:
 *
 * <pre>
 * {@code
 *  InputSource input = ...
 *  OutputStream output = ...
 *  WritableByteChannel wbc = Channels.newChannel(output);
 *  DaffodilUnparseContentHandler unparseContentHandler = dp.newContentHandlerInstance(wbc);
 *
 *  XMLReader xmlReader = ...
 *  xmlReader.setContentHandler(unparseContentHandler)
 *  try {
 *    xmlReader.parse(input);
 *  } catch (DaffodilUnparseErrorSAXException e) {
 *    // generally can be ignored, use getUnparseResult() instead
 *  } catch (SAXException e) {
 *    // non-Daffodil related exceptions created by the XMLReader, for example invalid XML
 *    ...
 *  } finally {
 *    unparseContentHandler.finish();
 *  }
 *  UnparseResult ur = unparseContentHandler.getUnparseResult();
 * }
 * </pre>
 *
 *
 * <h3>Failures and Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * {@link org.apache.daffodil.api.ProcessorFactory}, {@link org.apache.daffodil.api.DataProcessor},
 * {@link org.apache.daffodil.api.ParseResult}, or {@link org.apache.daffodil.api.UnparseResult}. However,
 * rather than throwing an exception on error (e.g. invalid DFDL schema, parse error, etc), these classes extend
 * {@link org.apache.daffodil.api.WithDiagnostics}, which is used to determine if an error occurred,
 * and any diagnostic information (see {@link org.apache.daffodil.api.Diagnostic}) related to the step.
 * Thus, before continuing, one must check {@link org.apache.daffodil.api.WithDiagnostics#isError}.
 * For example:
 *
 * <pre>
 * {@code
 * ProcessorFactor pf = c.compile(files);
 * if (pf.isError()) {
 *   java.util.List<Diagnostic> diags = pf.getDiagnostics();
 *   for (Diagnostic d : diags) {
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
 * {@link org.apache.daffodil.api.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = pf.onPath("/");
 * dp.save(saveFile);
 * }</pre>
 *
 * And to restore a saved {@link org.apache.daffodil.api.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = Daffodil.reload(saveFile);
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
 * ParseResult pr = xmlReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT());
 * }</pre>
 *
 * <h2>Thread Safety</h2>
 *
 * A compiled DFDL schema typically has a far larger memory footprint than the data it parses, so
 * it is usually worth compiling a schema once and sharing the result with as many threads as
 * needed. In short, share the compiled schema, but do not share anything that belongs to a single
 * parse or unparse.
 *
 * <p>The objects of this API fall into two groups: the immutable objects that describe the compiled
 * schema, which may be shared by any number of threads, and the stateful objects that belong to a
 * single parse or unparse, which must not be.
 *
 * <h3>Objects That May Be Shared Between Threads</h3>
 *
 * The following objects are immutable once created. Any number of threads may hold and call them at
 * the same time, with no additional synchronization:
 *
 * <ul>
 * <li>{@link org.apache.daffodil.api.Daffodil} - every static factory method of this class is
 * thread safe and may be called from any thread.</li>
 * <li>{@link org.apache.daffodil.api.Compiler}</li>
 * <li>{@link org.apache.daffodil.api.ProcessorFactory}</li>
 * <li>{@link org.apache.daffodil.api.DataProcessor}</li>
 * <li>{@link org.apache.daffodil.api.ParseResult} and {@link org.apache.daffodil.api.UnparseResult}
 * </ul>
 *
 * Because these objects are immutable, methods that appear to configure them do not change the
 * object they were called on. {@link org.apache.daffodil.api.Compiler#withTunable(String, String)},
 * {@link org.apache.daffodil.api.ProcessorFactory#withDistinguishedRootNode(String, String)},
 * {@link org.apache.daffodil.api.DataProcessor#withValidation(String, java.net.URL)}, and every
 * other {@code withX} method returns a new object. One thread may therefore create a new
 * configuration while other threads keep using the original.
 *
 * The methods that do the actual work,
 * {@link org.apache.daffodil.api.DataProcessor#parse(org.apache.daffodil.api.InputSourceDataInputStream, org.apache.daffodil.api.infoset.InfosetOutputter)}
 * and
 * {@link org.apache.daffodil.api.DataProcessor#unparse(org.apache.daffodil.api.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)},
 * may be called concurrently on a shared {@link org.apache.daffodil.api.DataProcessor}, mixing
 * parses and unparses in any combination. Everything that belongs to a single run, such as the bit
 * position in the data, the DFDL variables, the infoset being built, and the diagnostics, is created
 * fresh for each call, so concurrent runs cannot see or change each other's state. The same is true
 * of {@link org.apache.daffodil.api.DataProcessor#newXMLReaderInstance()},
 * {@link org.apache.daffodil.api.DataProcessor#newContentHandlerInstance},
 * {@link org.apache.daffodil.api.DataProcessor#save},
 * {@link org.apache.daffodil.api.DataProcessor#walkMetadata},
 * {@link org.apache.daffodil.api.ProcessorFactory#onPath(String)}, and the
 * {@code compileX} and {@code reload} methods of {@link org.apache.daffodil.api.Compiler}. Each call
 * to one of these creates independent state, so a single {@code DataProcessor} shared by a pool of
 * threads is the normal, recommended way to use Daffodil.
 *
 * <h3>Objects That Must Be Unique To Each Thread</h3>
 *
 * The following objects hold state for the duration of a run. They are not thread safe; each thread
 * must use its own instance, and an instance must not be given to another thread until every thread
 * that used it is finished with it:
 *
 * <ul>
 * <li>{@link org.apache.daffodil.api.InputSourceDataInputStream} - the current position in the data
 * lives in this object, so each parse needs its own. One shared instance would also interleave reads
 * of the underlying {@link java.io.InputStream}.</li>
 * <li>{@link org.apache.daffodil.api.infoset.InfosetOutputter} and
 * {@link org.apache.daffodil.api.infoset.InfosetInputter}, and therefore the infosets they build or
 * read. An {@link org.apache.daffodil.api.infoset.InfosetOutputter} may be reused by a thread, one
 * parse at a time, after calling {@link org.apache.daffodil.api.infoset.InfosetOutputter#reset()}.</li>
 * <li>{@link org.apache.daffodil.api.DaffodilParseXMLReader} and the handlers, resolvers and
 * properties registered on it. A thread that wants to parse data with SAX needs its own reader,
 * which is why {@link org.apache.daffodil.api.DataProcessor#newXMLReaderInstance()} is thread safe.</li>
 * <li>{@link org.apache.daffodil.api.DaffodilUnparseContentHandler}, for the same reason.</li>
 * <li>{@link org.apache.daffodil.api.debugger.Debugger} - a debugger is interactive and keeps track
 * of where the run it is attached to stopped, so a {@code DataProcessor} configured with a debugger
 * should be confined to one thread.</li>
 * <li>{@link org.apache.daffodil.api.CodeGenerator}</li>
 * </ul>
 *
 * Objects provided by the caller for a single run are subject to the same rule. The
 * {@link java.nio.channels.WritableByteChannel} given to
 * {@link org.apache.daffodil.api.DataProcessor#unparse(org.apache.daffodil.api.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)},
 * or the {@link java.io.OutputStream} given to one of the infoset text outputters, must be unique to
 * that call.
 *
 * <p>Objects that Daffodil calls back into have their own requirements, documented with them. The
 * {@link org.apache.daffodil.api.validation.Validator} and
 * {@link org.apache.daffodil.api.validation.ValidatorFactory} Service Provider Interface
 * implementations must be thread safe, because a single validator is used by all of the concurrent
 * parses that were configured with it. A {@link org.apache.daffodil.api.DaffodilEntityResolverProvider}
 * must also be thread safe, but the resolvers it returns are only ever used by one thread at a time.</p>
 *
 * <h3>Sharing Results Between Threads</h3>
 *
 * A {@link org.apache.daffodil.api.ParseResult} or {@link org.apache.daffodil.api.UnparseResult} is
 * only complete, and so only readable, once the call that returned it has finished. Passing the
 * result to another thread therefore needs the usual handoff, for example an
 * {@link java.util.concurrent.ExecutorService} and a {@link java.util.concurrent.Future}, which also
 * provides the memory visibility that the reading thread needs. The same applies to any infoset
 * built by an {@link org.apache.daffodil.api.infoset.InfosetOutputter}: build it on one thread, hand
 * it off, then read it on another.
 *
 * <h3>Multi-threaded Example</h3>
 *
 * <pre>
 * {@code
 * // compile once, on the main thread, and share the compiled schema with all of the workers
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(schemaFile);
 * if (pf.isError()) { ... } // no other thread is running yet, so nothing waits on the compilation
 * DataProcessor dp = pf.onPath("/");
 *
 * ExecutorService pool = Executors.newFixedThreadPool(4);
 * for (final File dataFile : dataFiles) {
 *   pool.submit(() -> {
 *     // both of these belong to this parse alone and are never seen by another thread
 *     try (InputSourceDataInputStream input =
 *              Daffodil.newInputSourceDataInputStream(new FileInputStream(dataFile))) {
 *       JDOMInfosetOutputter output = new JDOMInfosetOutputter();
 *
 *       ParseResult pr = dp.parse(input, output); // thread safe, dp is shared by all four threads
 *       if (pr.isError()) {
 *         ...
 *       } else {
 *         Document doc = output.getResult();
 *         ...
 *       }
 *     } catch (IOException e) {
 *       ...
 *     }
 *   });
 * }
 * }</pre>
 *
 */

package org.apache.daffodil.api;
