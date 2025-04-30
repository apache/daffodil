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

package org.apache.daffodil.core.dsom

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq

import org.apache.daffodil.core.grammar.primitives.AssertBooleanPrim
import org.apache.daffodil.core.grammar.primitives.AssertPatternPrim
import org.apache.daffodil.core.grammar.primitives.DiscriminatorBooleanPrim
import org.apache.daffodil.core.grammar.primitives.DiscriminatorPatternPrim
import org.apache.daffodil.lib.exceptions._
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType
import org.apache.daffodil.lib.schema.annotation.props.gen.TestKind

import com.ibm.icu.impl.UnicodeRegex

abstract class DFDLAssertionBase(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {

  /**
   * Perform checks that require the term on which this statement ultimately
   * resides..
   *
   * In this case, it's an assertion, so if it has a pattern we have to check that
   * pattern makes sense in the context.
   */
  override protected def check(term: Term) = checkPattern(term)

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

  private lazy val testPattern = {
    val optPattern = getAttributeOption("testPattern")
    optPattern.foreach { thePattern =>
      try {
        UnicodeRegex.compile(thePattern) // Check against ICU
        Pattern.compile(thePattern) // Check against Java
      } catch {
        case e: PatternSyntaxException =>
          SDE("The pattern contained invalid syntax: %s", e.getMessage())
      }
    }
    optPattern
  }

  /**
   * This assertion can appear on a global element def, for example, which is
   * not a term, and so we cannot ask it for properties and get all the properties
   * in the scope.
   *
   * To resolve properties you must start from the Term. (A local element decl,
   * an element ref, a sequence, choice, or group ref.)
   * To avoid reaching backward from a global element decl to the element ref
   * we instead call from a term to its statements, passing them the Term they need to verify
   * that they are compatible with the properties of that term.
   *
   * This is a technique to avoid backpointers from shared definitions/decls to
   * the referencing objects that refer to them.
   */
  private def checkPattern(term: Term) = {

    if (testPattern.isDefined) {
      val thePattern = testPattern.get

      val hasWord = thePattern.contains("\\w")
      val encInfo = term.termRuntimeData.encodingInfo
      if (encInfo.knownEncodingIsUnicode && hasWord)
        SDW(
          WarnID.PatternEncodingSlashW,
          "The encoding is '%s' and \\w was detected in the pattern '%s'.  This is not recommended with Unicode encodings.",
          encInfo.knownEncodingName,
          thePattern
        )
    }
  }

  final lazy val testKind = getAttributeOption("testKind") match {
    case Some(str) => TestKind(str, decl)
    case None => TestKind.Expression
  }

  final lazy val failureType = getAttributeOption("failureType") match {
    case Some(str) => FailureType(str, decl)
    case None => FailureType.ProcessingError
  }

  lazy val messageAttrib = getAttributeOption("message")

  final lazy val testTxt = {
    val rawTxt = (testKind, testBody, testAttrib, testPattern) match {
      case (TestKind.Expression, None, Some(txt), None) => txt
      case (TestKind.Expression, txt, None, None) => txt.get
      case (TestKind.Pattern, _, _, Some("")) =>
        SDE("The attribute testPattern must not be empty for testKind='pattern'")
      case (TestKind.Pattern, None, None, Some(pat)) => pat
      case (TestKind.Expression, Some(bdy), Some(attrib), _) =>
        SDE("You may not specify both test attribute and a body expression.")
      case (TestKind.Expression, None, None, _) =>
        SDE("You must specify either a test attribute or a body expression.")
      case (TestKind.Pattern, Some(bdy), _, Some(txt)) =>
        SDE("You may not specify both testPattern attribute and a body expression.")
      case (TestKind.Pattern, None, _, None) =>
        SDE(
          "You must specify either a testPattern attribute or a body expression. for testKind='pattern'"
        )
      case (TestKind.Pattern, Some(bdy), None, None) => bdy // pattern as body of assert element
      case (TestKind.Pattern, _, Some(tst), _) =>
        SDE("You cannot specify test='%s' for testKind='pattern'", tst)
      case (TestKind.Expression, _, _, Some(pat)) =>
        SDE(
          "You cannot specify testPattern='%s' for testKind='expression' (which is the default test kind.)",
          pat
        )
      case _ => Assert.invariantFailed("unexpected case.")
    }
    // we need to be sure if it is an expression that it is surrounded by {...}
    // after trimming whitespace from before and after. Jira issue DFDL-434
    if (testKind == TestKind.Expression)
      schemaDefinitionUnless(
        rawTxt.startsWith("{") && !rawTxt.startsWith("{{") && rawTxt.endsWith("}"),
        "Expression must begin with a single '{' and end with a '}'"
      )
    rawTxt
  }
}

final class DFDLAssert(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Assert_AnnotationMixin // Note: don't use these generated mixins. Statements don't have format properties

  final def gram(term: Term) = LV(Symbol("gram")) {
    testKind match {
      case TestKind.Pattern => AssertPatternPrim(term, this)
      case TestKind.Expression => AssertBooleanPrim(term, this)
    }
  }.value
}

final class DFDLDiscriminator(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Discriminator_AnnotationMixin

  final def gram(term: Term) = LV(Symbol("gram")) {
    testKind match {
      case TestKind.Pattern => DiscriminatorPatternPrim(term, this)
      case TestKind.Expression => DiscriminatorBooleanPrim(term, this)
    }
  }.value
}
