/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The {@link edu.illinois.ncsa.daffodil.japi.Daffodil} object is a factory object to create a {@link edu.illinois.ncsa.daffodil.japi.Compiler}. The
 * {@link edu.illinois.ncsa.daffodil.japi.Compiler} provides a method to compils a provided DFDL schema into a
 * {@link edu.illinois.ncsa.daffodil.japi.ProcessorFactory}, which creates a {@link edu.illinois.ncsa.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(file);
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * The {@link edu.illinois.ncsa.daffodil.japi.DataProcessor} provides the necessary functions to parse and unparse
 * data, returning a {@link edu.illinois.ncsa.daffodil.japi.ParseResult} or {@link edu.illinois.ncsa.daffodil.japi.UnparseResult}, respectively. These
 * contain information about the parse/unparse, such as whether or not the
 * processing succeeded any diagnostic information.
 *
 * <h4>Parse</h4>
 *
 * The {@link edu.illinois.ncsa.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter, long)} method accepts input data to parse in the form
 * of a {@link java.nio.channels.ReadableByteChannel} and an {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter}
 * to determine the output representation of the infoset (e.g. Scala XML Nodes,
 * JDOM2 Documents, etc.):
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter= new JDOMInfosetOutputter();
 * ParseResult pr = dp.parse(data, jdomOutputter);
 * Document doc = jdomOutputter.getResult();
 * }</pre>
 *
 * The {@link edu.illinois.ncsa.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter, long)} method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter}'s are not thread safe, requiring a unique instance per
 * thread. An {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter} should call {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter#reset()} before
 * reuse (or a new one should be allocated). For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter = new JDOMInfosetOutputter();
 * for (File f : inputFiles) {
 *   jdomOutputter.reset();
 *   ParseResult pr = dp.parse(f, jdomOutputter);
 *   Document doc = jdomOutputter.getResult();
 * }
 * }</pre>
 *
 * <h4>Unparse</h4>
 *
 * The same {@link edu.illinois.ncsa.daffodil.japi.DataProcessor} used for parse can be used to unparse an infoset
 * via the {@link edu.illinois.ncsa.daffodil.japi.DataProcessor#unparse(edu.illinois.ncsa.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method. An {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetInputter}
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
 * {@link edu.illinois.ncsa.daffodil.japi.ProcessorFactory}, {@link edu.illinois.ncsa.daffodil.japi.DataProcessor}, or {@link edu.illinois.ncsa.daffodil.japi.ParseResult}. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend {@link edu.illinois.ncsa.daffodil.japi.WithDiagnostics}, which is used to
 * determine if an error occurred, and any diagnostic information (see
 * {@link edu.illinois.ncsa.daffodil.japi.Diagnostic}) related to the step. Thus, before continuing, one must check
 * {@link edu.illinois.ncsa.daffodil.japi.WithDiagnostics#isError}. For example:
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
 * {@link edu.illinois.ncsa.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = pf.onPath("/");
 * dp.save(saveFile);
 * }</pre>
 *
 * And to restore a saved {@link edu.illinois.ncsa.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = Daffodil.reload(saveFile);
 * ParseResult pr = dp.parse(data);
 * }</pre>
 *
 */

package edu.illinois.ncsa.daffodil.japi;

