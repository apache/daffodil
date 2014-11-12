/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The main class to use is {@link edu.illinois.ncsa.daffodil.japi.Daffodil} to
 * create a {@link edu.illinois.ncsa.daffodil.japi.Compiler}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * }</pre>
 *
 * This can then be used to compiled a DFDL schema, and generate a
 * {@link edu.illinois.ncsa.daffodil.japi.ProcessorFactory}:
 *
 * <pre>
 * {@code
 * ProcessorFactor pf = c.compile(files);
 * }</pre>
 *
 * This can then be used to create a {@link
 * edu.illinois.ncsa.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * This can then be used to parse data, returning a {@link
 * edu.illinois.ncsa.daffodil.japi.ParseResult}, which contains the
 * DFDL infoset in the form of a jdom2 document:
 *
 * <pre>
 * {@code
 * ParseResult pr = dp.parse(data);
 * org.jdom2.Document infoset = pr.result();
 * }</pre>
 *
 * The {@link
 * edu.illinois.ncsa.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel)}
 * method may be called multiple times without the need to create
 * another data processors. For example:
 *
 * <pre>
 * {@code
 * for (File f : inputFiles) {
 *   ParseResult pr = dp.parse(f);
 *   org.jdom2.Document infoset = pr.result();
 * }
 * }</pre>
 *
 * <h3>Failures &amp; Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * {@link edu.illinois.ncsa.daffodil.japi.ProcessorFactory}, {@link
 * edu.illinois.ncsa.daffodil.japi.DataProcessor}, or {@link
 * edu.illinois.ncsa.daffodil.japi.ParseResult}. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend {@link
 * edu.illinois.ncsa.daffodil.japi.WithDiagnostics}, which is used to
 * determine if an error occured, and any diagnostic information (see
 * {@link edu.illinois.ncsa.daffodil.japi.Diagnostic}) related to the
 * step. thus, before contining, one must check {@link
 * edu.illinois.ncsa.daffodil.japi.WithDiagnostics#isError}. For
 * example:
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
 * <h3>Saving &amp; Reloading Parsers</h3>
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
 * And to restore a saved {@link
 * edu.illinois.ncsa.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = Daffodil.reload(saveFile);
 * ParseResult pr = dp.parse(data);
 * }</pre>
 *
 */

package edu.illinois.ncsa.daffodil.japi;

