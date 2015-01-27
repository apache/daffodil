package edu.illinois.ncsa.daffodil

/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The main class to use is [[Daffodil]] to
 * create a [[Compiler]]:
 *
 * {{{
 * val c = Daffodil.compiler()
 * }}}
 *
 * This can then be used to compiled a DFDL schema, and generate a
 * [[ProcessorFactory]]:
 *
 * {{{
 * val pf = c.compileFile(file)
 * }}}
 *
 * This can then be used to create a [[DataProcessor]]:
 *
 * {{{
 * val dp = pf.onPath("/")
 * }}}
 *
 * This can then be used to parse data, returning a [[ParseResult]], which contains the
 * DFDL infoset in either a jdom2 Document or a scala XML Node:
 *
 * {{{
 * val pr = dp.parse(data)
 * val infoset = pr.result()
 * }}}
 *
 * The [[DataProcessor.parse(java.nio.channels.ReadableByteChannel)]]
 * method may be called multiple times without the need to create
 * another data processors. For example:
 *
 * {{{
 * files.foreach { f => {
 *   val pr = dp.parse(f)
 *   val infoset = pr.result()
 * }}
 * }}}
 *
 * <h3>Failures &amp; Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * [[ProcessorFactory]], [[DataProcessor]], or [[ParseResult]]. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend [[WithDiagnostics]], which is used to
 * determine if an error occured, and any diagnostic information (see
 * [[Diagnostic]]) related to the
 * step. thus, before contining, one must check [[WithDiagnostics#isError]]. For
 * example:
 *
 * {{{
 * val pf = c.compile(file)
 * if (pf.isError()) {
 *   val diags = pf.getDiagnostics()
 *   diags.foreach { d =>
 *     System.out.println(d.toString())
 *   }
 *   return -1;
 * }
 * }}}
 *
 * <h3>Saving &amp; Reloading Parsers</h3>
 *
 * In some cases, it may be beneficial to save a parser and reload it.
 * For example, when starting up, it may be quicker to reload an
 * already compiled parser than to compile it from scratch. To save a
 * [[DataProcessor]]:
 *
 * {{{
 * val dp = pf.onPath("/")
 * dp.save(saveFile);
 * }}}
 *
 * And to restore a saved [[DataProcessor]]:
 *
 * {{{
 * val dp = Daffodil.reload(saveFile);
 * val pr = dp.parse(data);
 * }}}
 *
 */
package object sapi

