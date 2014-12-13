package edu.illinois.ncsa.daffodil.dsom

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

import scala.collection.immutable.ListMap
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeScheme_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.VariableUtils

abstract class DFDLAssertionBase(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {
  private lazy val testAttrib = getAttributeOption("test")

  // tolerate whitespace. E.g., 
  //   <assert test="...">
  //   </assert>
  // on two lines, indented, has whitespace in the body of the element.
  // Just reformatting XML, or printing it out with a pretty printer
  // can break assertions otherwise.
  //
  // even if you write <assert><![CDATA[{ ... }]]></assert> 
  // you can still lose because the implementation might convert
  // the schema to a string (by pretty printing), and this may re-insert 
  // whitespace.
  //
  // So, we trim the body string.
  private[dsom] lazy val testBody: Option[String] = node.child.text match {
    case s if (s.trim().length() == 0) => None
    case txt => Some(txt.trim())
  } // package visible for unit testing

  private lazy val testPattern = getAttributeOption("testPattern")
  lazy val testKind = getAttributeOption("testKind") match {
    case Some(str) => TestKind(str, decl)
    case None => TestKind.Expression
  }

  private lazy val messageAttrib = getAttributeOption("message")
  lazy val message = messageAttrib match {
    case None => "%s failed".format(testTxt)
    case Some(s) => s
  }

  lazy val testTxt = {
    val rawTxt = (testKind, testBody, testAttrib, testPattern) match {
      case (TestKind.Expression, None, Some(txt), None) => txt
      case (TestKind.Expression, txt, None, None) => txt.get
      case (TestKind.Pattern, _, _, Some("")) => SDE("The attribute testPattern must not be empty for testKind='pattern'")
      case (TestKind.Pattern, None, None, Some(pat)) => pat
      case (TestKind.Expression, Some(bdy), Some(attrib), _) => SDE("You may not specify both test attribute and a body expression.")
      case (TestKind.Expression, None, None, _) => SDE("You must specify either a test attribute or a body expression.")
      case (TestKind.Pattern, Some(bdy), _, Some(txt)) => SDE("You may not specify both testPattern attribute and a body expression.")
      case (TestKind.Pattern, None, _, None) => SDE("You must specify either a testPattern attribute or a body expression. for testKind='pattern'")
      case (TestKind.Pattern, Some(bdy), None, None) => bdy // pattern as body of assert element
      case (TestKind.Pattern, _, Some(tst), _) => SDE("You cannot specify test='%s' for testKind='pattern'", tst)
      case (TestKind.Expression, _, _, Some(pat)) => SDE("You cannot specify testPattern='%s' for testKind='expression' (which is the default test kind.)", pat)
      case _ => Assert.invariantFailed("unexpected case.")
    }
    // we need to be sure if it is an expression that it is surrounded by {...}
    // after trimming whitespace from before and after. Jira issue DFDL-434
    if (testKind == TestKind.Expression)
      schemaDefinitionUnless(rawTxt.startsWith("{") && !rawTxt.startsWith("{{") && rawTxt.endsWith("}"),
        "Expression must begin with a single '{' and end with a '}'")
    rawTxt
  }
}

class DFDLAssert(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Assert_AnnotationMixin // Note: don't use these generated mixins. Statements don't have format properties
  lazy val gram = gram_.value
  private val gram_ = LV('gram) {
    testKind match {
      case TestKind.Pattern => AssertPatternPrim(decl, this)
      case TestKind.Expression => AssertBooleanPrim(decl, this)
    }
  }
}

class DFDLDiscriminator(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Discriminator_AnnotationMixin 
  lazy val gram = gram_.value
  private val gram_ = LV('gram) {
    testKind match {
      case TestKind.Pattern => DiscriminatorPatternPrim(decl, this)
      case TestKind.Expression => DiscriminatorBooleanPrim(decl, this)
    }
  }
}

