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

package org.apache.daffodil.runtime1

import org.apache.daffodil.xml.QNameBase
import org.apache.daffodil.infoset.SeveralPossibilitiesForNextElement
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.infoset.NoNextElement
import org.apache.daffodil.infoset.OnlyOnePossibilityForNextElement
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.ModelGroup
import org.apache.daffodil.dsom.DPathElementCompileInfo
import org.apache.daffodil.dsom.PrefixLengthQuasiElementDecl
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.schema.annotation.props.gen.NilKind
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.schema.annotation.props.gen.LengthKind

/**
 * Part of Daffodil's Runtime 1 Streaming Unparser Support.
 *
 * When streaming in some representation of an infoset for unparsing,
 * Daffodil (in Runtime 1) must resolve what element to construct.
 * This is context specific, and given sharing of elements and groups,
 * requires a runtime stack of TermRuntimeData, each stack entry has
 * an associated PartialNextElementResolver.
 *
 * This object computes that PartialNextElementResolver based on the
 * possible elements that can follow the current term.
 */
trait TermRuntime1Mixin { self: Term =>

  def termRuntimeData: TermRuntimeData

  /**
   * Set of elements referenced from an expression in the scope of this term.
   *
   * Specific to certain function call contexts e.g., only elements referenced
   * by dfdl:valueLength or dfdl:contentLength.
   *
   * Separated by parser/unparser since parsers have to derive from
   * dfdl:inputValueCalc, and must include discriminators and assert test
   * expressions. Unparsers must derive from dfdl:outputValueCalc and exclude
   * discriminators and asserts. Both must include setVariable/newVariableInstance,
   * and property expressions are nearly the same. There are some unparser-specfic
   * properties that take runtime-valued expressions - dfdl:outputNewLine is
   * one example.
   */
  final lazy val contentLengthParserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyContentReferencedElementInfos
    val stmtRefs = statementContentParserReferencedElementInfos
    val calcRefs = calcContentParserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.contentLengthParserReferencedElementInfos) }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val contentLengthUnparserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyContentReferencedElementInfos
    val stmtRefs = statementContentUnparserReferencedElementInfos
    val calcRefs = calcContentUnparserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.contentLengthUnparserReferencedElementInfos) }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val valueLengthParserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyValueReferencedElementInfos
    val stmtRefs = statementValueParserReferencedElementInfos
    val calcRefs = calcValueParserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.valueLengthParserReferencedElementInfos) }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val valueLengthUnparserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyValueReferencedElementInfos
    val stmtRefs = statementValueUnparserReferencedElementInfos
    val calcRefs = calcValueUnparserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.valueLengthUnparserReferencedElementInfos) }
    res
  }

}
