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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom.Choice
import org.apache.daffodil.core.dsom.ChoiceGroupRef
import org.apache.daffodil.core.dsom.ElementRef
import org.apache.daffodil.core.dsom.LocalElementDecl
import org.apache.daffodil.core.dsom.LocalSequence
import org.apache.daffodil.core.dsom.SequenceGroupRef
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.core.util.TestUtils.getRoot

import org.junit.Assert.fail
import org.junit.Test

class TestStreamingUnparserCompilerAttributes {

  import PossibleNextElements.*

  private def poss(t: Term) = t.possibleNextLexicalSiblingStreamingUnparserElements
  type LE = LocalElementDecl
  type SGR = SequenceGroupRef
  type CGR = ChoiceGroupRef
  type S = LocalSequence
  type C = Choice
  type ER = ElementRef

  @Test def testPossibleNextStreamingUnparserEvent1() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int"/>
            <xs:element name="b" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    }
    val rg: S = r.complexType.sequence
    val (a, b) = rg.groupMembers match {
      case Seq(a: LE, b: LE) => (a, b)
      case _ => fail(); null
    }
    poss(r) match {
      case Closed(Seq(PNE(`r`, true))) =>
      case _ => fail()
    }
    poss(rg) match {
      case Closed(Nil) =>
      case _ => fail()
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent2() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int" minOccurs="0"/>
            <xs:element name="b" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    }
    val rg: S = r.complexType.sequence
    val (a, b) = rg.groupMembers match {
      case Seq(a: LE, b: LE) => (a, b)
      case _ => fail(); null

    }
    poss(r) match {
      case Closed(Seq(PNE(`r`, true))) =>
      case _ => fail()
    }
    poss(rg) match {
      case Closed(Nil) =>
      case _ => fail()
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, false), PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent2OVC() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int" dfdl:outputValueCalc="{ 0 }"/>
            <xs:element name="b" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    }
    val rg: S = r.complexType.sequence
    val (a, b) = rg.groupMembers match {
      case Seq(a: LE, b: LE) => (a, b)
      case _ => fail(); null
    }
    poss(r) match {
      case Closed(Seq(PNE(`r`, true))) =>
      case _ => fail()
    }
    poss(rg) match {
      case Closed(Nil) =>
      case _ => fail()
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, false), PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent3() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int" minOccurs="0"/>
            <xs:element name="b" type="xs:int" maxOccurs="unbounded"/>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="c" type="xs:int"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, b: LE, gr: SGR) = rg.groupMembers match {
      case Seq(a: LE, b: LE, gr: SGR) => (a: LE, b: LE, gr: SGR)
      case _ => fail(); null
    }
    val (c: LE) = gr.groupMembers match {
      case Seq(c: LE) => (c: LE)
      case _ => fail(); null
    }
    poss(r) match {
      case Closed(Seq(PNE(`r`, true))) =>
      case _ => fail()
    }
    poss(rg) match {
      case Closed(Nil) =>
      case _ => fail()
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, false), PNE(`b`, false), PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, false), PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent4() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int" minOccurs="0"/>
            <xs:element name="b" type="xs:int" maxOccurs="unbounded"/>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:choice>
          <xs:element name="c" type="xs:int"/>
          <xs:element name="d" type="xs:int"/>
        </xs:choice>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, b: LE, gr: CGR) = rg.groupMembers match {
      case Seq(a: LE, b: LE, gr: CGR) => (a: LE, b: LE, gr: CGR)
      case _ => fail(); null
    }
    val (c: LE, d: LE) = gr.groupMembers match {
      case Seq(c: LE, d: LE) => (c: LE, d: LE)
      case _ => fail(); null
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, false), PNE(`b`, false), PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, false), PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(gr) match {
      case Closed(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent5() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:group ref="gab"/>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="gab">
        <xs:sequence>
          <xs:element name="a" type="xs:int" minOccurs="0"/>
          <xs:element name="b" type="xs:int" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:group>
      <xs:group name="g">
        <xs:choice>
          <xs:element name="c" type="xs:int"/>
          <xs:element name="d" type="xs:int"/>
        </xs:choice>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (sgr: SGR, cgr: CGR) = rg.groupMembers match {
      case Seq(sgr: SGR, cgr: CGR) => (sgr: SGR, cgr: CGR)
      case _ => fail(); null
    }
    val (a: LE, b: LE) = sgr.groupMembers match {
      case Seq(a: LE, b: LE) => (a: LE, b: LE)
      case _ => fail(); null
    }
    val (c: LE, d: LE) = cgr.groupMembers match {
      case Seq(c: LE, d: LE) => (c: LE, d: LE)
      case _ => fail(); null
    }
    poss(sgr) match {
      case Closed(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(a) match {
      case Open(Seq(PNE(`a`, false), PNE(`b`, false))) =>
      case _ => fail()
    }
    poss(b) match {
      case Open(Seq(PNE(`b`, false))) =>
      case _ => fail()
    }
    poss(cgr) match {
      case Closed(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent6() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE) = rg.groupMembers match {
      case Seq(c: LE, d: LE) => (c: LE, d: LE)
      case _ => fail(); null
    }
    poss(rg) match {
      case Closed(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent7() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int"/>
            <xs:sequence/><!-- added empty sequence as branch -->
          </xs:choice>
        </xs:complexType>
      </xs:element>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE, es: S) = rg.groupMembers match {
      case Seq(c: LE, d: LE, es: S) => (c: LE, d: LE, es: S)
      case _ => fail(); null
    }
    poss(es) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(rg) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent8() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int"/>
            <xs:group ref="g"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:sequence/>
      </xs:group>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE, gr: SGR) = rg.groupMembers match {
      case Seq(c: LE, d: LE, gr: SGR) => (c: LE, d: LE, gr: SGR)
      case _ => fail(); null
    }
    poss(gr) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(rg) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent9() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int"/>
            <xs:group ref="g"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="e" type="xs:int" maxOccurs="2"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE, gr: SGR) = rg.groupMembers match {
      case Seq(c: LE, d: LE, gr: SGR) => (c: LE, d: LE, gr: SGR)
      case _ => fail(); null
    }
    val (e: LE) = gr.groupMembers match {
      case Seq(e: LE) => (e: LE)
      case _ => fail(); null
    }
    poss(gr) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(rg) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent9OVC() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int"/>
            <xs:group ref="g"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:outputValueCalc="{ 0 }"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE, gr: SGR) = rg.groupMembers match {
      case Seq(c: LE, d: LE, gr: SGR) => (c: LE, d: LE, gr: SGR)
      case _ => fail(); null
    }
    val (e: LE) = gr.groupMembers match {
      case Seq(e: LE) => (e: LE)
      case _ => fail(); null
    }
    poss(gr) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(rg) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent9OVCIVC() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:choice>
            <xs:element name="c" type="xs:int"/>
            <xs:element name="d" type="xs:int" dfdl:inputValueCalc="{ 0 }"/><!-- IVC changes nothing as far as unparsing goes -->
            <xs:group ref="g"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:outputValueCalc="{ 0 }"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: C = r.complexType.choice
    val (c: LE, d: LE, gr: SGR) = rg.groupMembers match {
      case Seq(c: LE, d: LE, gr: SGR) => (c: LE, d: LE, gr: SGR)
      case _ => fail(); null
    }
    val (e: LE) = gr.groupMembers match {
      case Seq(e: LE) => (e: LE)
      case _ => fail(); null
    }
    poss(gr) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(rg) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent10() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:group ref="cg"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="cg">
        <xs:choice>
          <xs:element name="c" type="xs:int"/>
          <xs:element name="d" type="xs:int"/>
          <xs:group ref="g"/>
        </xs:choice>
      </xs:group>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="e" type="xs:int" maxOccurs="2"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (s: S) = rg.groupMembers match {
      case Seq(s: S) => (s: S)
      case _ => fail(); null
    }
    val (cgr: CGR) = s.groupMembers match {
      case Seq(cgr: CGR) => (cgr: CGR)
      case _ => fail(); null
    }
    val (c: LE, d: LE, gr: SGR) = cgr.groupMembers match {
      case Seq(c: LE, d: LE, gr: SGR) => (c: LE, d: LE, gr: SGR)
      case _ => fail(); null
    }
    val (e: LE) = gr.groupMembers match {
      case Seq(e: LE) => (e: LE)
      case _ => fail(); null
    }
    poss(rg) match {
      case Closed(Nil) =>
      case _ => fail()
    }
    poss(s) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(cgr) match {
      case Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) =>
      case _ => fail()
    }
    poss(gr) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(d) match {
      case Closed(Seq(PNE(`d`, true))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEvent11() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int"/>
            <xs:element name="b" type="xs:int" maxOccurs="unbounded"/>
            <xs:element name="c" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, b: LE, c: LE) = rg.groupMembers match {
      case Seq(a: LE, b: LE, c: LE) => (a: LE, b: LE, c: LE)
      case _ => fail(); null
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, false), PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
  }

// DAFFODIL-2937
  @Test def testPossibleNextStreamingUnparserEvent12() = {
    val r = getRoot {
      <xs:element name="root">
        <xs:complexType>
          <xs:group ref="ex:g2" dfdl:separator="%NL;"></xs:group>
        </xs:complexType>
      </xs:element>
        <xs:group name="g2">
          <xs:sequence dfdl:separatorPosition="infix">
            <xs:element name="g2c0" type="xs:string" />
            <xs:element name="g2c1" type="xs:string" />
            <xs:element name="g2c2" type="xs:string" maxOccurs="unbounded" />
            <xs:sequence dfdl:hiddenGroupRef="ex:hg" />
            <xs:group ref="ex:vg" />
          </xs:sequence>
        </xs:group>
        <xs:group name="hg">
          <xs:choice>
            <xs:element name="hg_inty" type="xs:int"
                        dfdl:lengthKind="delimited" dfdl:outputValueCalc="{ 1 }" />
            <xs:element name="hg_stringy" type="xs:string"
                        dfdl:lengthKind="delimited" />
          </xs:choice>
        </xs:group>
        <xs:group name="vg">
          <xs:choice>
            <xs:element name="vg_inty" type="xs:int"
                        dfdl:lengthKind="delimited" />
            <xs:element name="vg_stringy" type="xs:string"
                        dfdl:lengthKind="delimited" />
          </xs:choice>
        </xs:group>
    }
    val rg: SGR = r.complexType.group.asInstanceOf[SequenceGroupRef]
    val (g2c1: LE, g2c2: LE, g2c3: LE, hg: CGR, vg: CGR) = rg.groupMembers match {
      case Seq(g2c1: LE, g2c2: LE, g2c3: LE, hg: CGR, vg: CGR) =>
        (g2c1: LE, g2c2: LE, g2c3: LE, hg: CGR, vg: CGR)
      case _ => fail(); null
    }
    poss(g2c1) match {
      case Closed(Seq(PNE(`g2c1`, true))) =>
      case _ => fail()
    }
    poss(g2c2) match {
      case Closed(Seq(PNE(`g2c2`, true))) =>
      case _ => fail()
    }
    val (vg_inty, vg_stringy) = poss(g2c3) match {
      case Closed(Seq(PNE(`g2c3`, false), PNE(vg_inty, false), PNE(vg_stringy, false))) =>
        (vg_inty, vg_stringy)
      case _ => fail(); null
    }
    poss(hg) match {
      case Open(Seq(PNE(`vg_inty`, false), PNE(`vg_stringy`, false))) =>
      case _ => fail()
    }
    poss(vg) match {
      case Closed(Seq(PNE(`vg_inty`, false), PNE(`vg_stringy`, false))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEventHidden1() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int"/>
            <xs:sequence dfdl:hiddenGroupRef="ex:hg"/>
            <xs:element name="b" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="hg">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:outputValueCalc="{ 0 }"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, hgr: SGR, b: LE) = rg.groupMembers match {
      case Seq(a: LE, hgr: SGR, b: LE) => (a: LE, hgr: SGR, b: LE)
      case _ => fail(); null
    }
    val (e: LE) = hgr.groupMembers match {
      case Seq(e: LE) => (e: LE)
      case _ => fail(); null
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, true))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(hgr) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(e) match {
      case Open(Seq(PNE(`e`, false))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEventHidden2() = {
    val r = getRoot {
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:int"/>
            <xs:choice>
              <xs:sequence>
                <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
                <xs:element name="b" type="xs:int"/>
              </xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
            </xs:choice>
            <xs:element name="c" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="presentBit">
        <xs:sequence>
          <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:lengthUnits="bits" dfdl:outputValueCalc="{ 1 }"/>
        </xs:sequence>
      </xs:group>
      <xs:group name="absentBit">
        <xs:sequence>
          <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:lengthUnits="bits" dfdl:outputValueCalc="{ 0 }"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, cg: C, c: LE) = rg.groupMembers match {
      case Seq(a: LE, cg: C, c: LE) => (a: LE, cg: C, c: LE)
      case _ => fail(); null
    }
    val (s1: S, s2: SGR) = cg.groupMembers match {
      case Seq(s1: S, s2: SGR) => (s1: S, s2: SGR)
      case _ => fail(); null
    }
    val (pg: SGR, b: LE) = s1.groupMembers match {
      case Seq(pg: SGR, b: LE) => (pg: SGR, b: LE)
      case _ => fail(); null
    }
    val (aflag: LE) = s2.groupMembers match {
      case Seq(aflag: LE) => (aflag: LE)
      case _ => fail(); null
    }
    val (pflag: LE) = pg.groupMembers match {
      case Seq(pflag: LE) => (pflag: LE)
      case _ => fail(); null
    }
    poss(a) match {
      case Closed(Seq(PNE(`a`, true))) =>
      case _ => fail()
    }
    poss(cg) match {
      case Closed(Seq(PNE(`b`, false), PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(s1) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(s2) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(pg) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(pflag) match {
      case Open(Seq(PNE(`pflag`, false))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(aflag) match {
      case Open(Seq(PNE(`aflag`, false))) =>
      case _ => fail()
    }
  }

  @Test def testPossibleNextStreamingUnparserEventHidden3() = {
    val r = getRoot {
      <xs:element name="r" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:length="3"/>
            <xs:choice>
              <xs:sequence>
                <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
                <xs:element name="b" type="xs:string" dfdl:length="3"/>
              </xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
            </xs:choice>
            <xs:choice>
              <xs:sequence>
                <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
                <xs:element name="c" type="xs:string" dfdl:length="3"/>
              </xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
            </xs:choice>
            <xs:element name="d" type="xs:string" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="presentBit">
        <xs:sequence>
          <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }"/>
        </xs:sequence>
      </xs:group>
      <xs:group name="absentBit">
        <xs:sequence>
          <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
        </xs:sequence>
      </xs:group>
    }
    val rg: S = r.complexType.sequence
    val (a: LE, cg1: C, cg2: C, d: LE) = rg.groupMembers match {
      case Seq(a: LE, cg1: C, cg2: C, d: LE) => (a: LE, cg1: C, cg2: C, d: LE)
      case _ => fail(); null
    }
    val (s1a: S, s1b: SGR) = cg1.groupMembers match {
      case Seq(s1a: S, s1b: SGR) => (s1a: S, s1b: SGR)
      case _ => fail(); null
    }
    val (pg1: SGR, b: LE) = s1a.groupMembers match {
      case Seq(pg1: SGR, b: LE) => (pg1: SGR, b: LE)
      case _ => fail(); null
    }
    val (aflag1: LE) = s1b.groupMembers match {
      case Seq(aflag1: LE) => (aflag1: LE)
      case _ => fail(); null
    }
    val (pflag1: LE) = pg1.groupMembers match {
      case Seq(pflag1: LE) => (pflag1: LE)
      case _ => fail(); null
    }

    val (s2a: S, s2b: SGR) = cg2.groupMembers match {
      case Seq(s2a: S, s2b: SGR) => (s2a: S, s2b: SGR)
      case _ => fail(); null
    }
    val (pg2: SGR, c: LE) = s2a.groupMembers match {
      case Seq(pg2: SGR, c: LE) => (pg2: SGR, c: LE)
      case _ => fail(); null
    }
    val (aflag2: LE) = s2b.groupMembers match {
      case Seq(aflag2: LE) => (aflag2: LE)
      case _ => fail(); null
    }
    val (pflag2: LE) = pg2.groupMembers match {
      case Seq(pflag2: LE) => (pflag2: LE)
      case _ => fail(); null
    }

    poss(a) match {
      case Closed(Seq(PNE(`a`, true))) =>
      case _ => fail()
    }

    poss(cg1) match {
      case Closed(Seq(PNE(`b`, false), PNE(`c`, false), PNE(`d`, true))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(s1a) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(s1b) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(pg1) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(pflag1) match {
      case Open(Seq(PNE(`pflag1`, false))) =>
      case _ => fail()
    }
    poss(b) match {
      case Closed(Seq(PNE(`b`, true))) =>
      case _ => fail()
    }
    poss(aflag1) match {
      case Open(Seq(PNE(`aflag1`, false))) =>
      case _ => fail()
    }

    poss(cg2) match {
      case Closed(Seq(PNE(`c`, false), PNE(`d`, true))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(s2a) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(s2b) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(pg2) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(pflag2) match {
      case Open(Seq(PNE(`pflag2`, false))) =>
      case _ => fail()
    }
    poss(c) match {
      case Closed(Seq(PNE(`c`, true))) =>
      case _ => fail()
    }
    poss(aflag2) match {
      case Open(Seq(PNE(`aflag2`, false))) =>
      case _ => fail()
    }
  }

  val schemaX =
    <xs:element name="x" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:choice>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="tns:present"/>
              <xs:element maxOccurs="2" name="b" type="xs:int" dfdl:length="1"/>
            </xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="tns:absent"/>
          </xs:choice>
          <xs:element name="y" minOccurs="0" type="xs:int" dfdl:length="1"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:group name="present">
      <xs:sequence>
        <xs:element name="present" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }">
          <xs:annotation>
            <xs:appinfo source="http://www.ogf.org/dfdl/">
              <dfdl:discriminator test="{ . eq 1 }"/>
            </xs:appinfo>
          </xs:annotation>
        </xs:element>
      </xs:sequence>
    </xs:group>
    <xs:group name="absent">
      <xs:sequence>
        <xs:element name="absent" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
      </xs:sequence>
    </xs:group>

  @Test def testUnparseHiddenGroupsPresenceFlags6(): Unit = {
    val r =
      getRoot(schemaX, topLevels = <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>)

    val rg: S = r.complexType.sequence
    val (ch: C, y: LE) = rg.groupMembers match {
      case Seq(ch: C, y: LE) => (ch: C, y: LE)
      case _ => fail(); null
    }
    val (s1: S, ag: SGR) = ch.groupMembers match {
      case Seq(s1: S, ag: SGR) => (s1: S, ag: SGR)
      case _ => fail(); null
    }
    val (pg: SGR, b: LE) = s1.groupMembers match {
      case Seq(pg: SGR, b: LE) => (pg: SGR, b: LE)
      case _ => fail(); null
    }
    val (p: LE) = pg.groupMembers match {
      case Seq(p: LE) => (p: LE)
      case _ => fail(); null
    }
    val (a: LE) = ag.groupMembers match {
      case Seq(a: LE) => (a: LE)
      case _ => fail(); null
    }

    poss(ch) match {
      case Open(Seq(PNE(`b`, false), PNE(`y`, false))) =>
      case _ => fail()
    }
    poss(s1) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(y) match {
      case Open(Seq(PNE(`y`, false))) =>
      case _ => fail()
    }
    poss(pg) match {
      case Open(Seq(PNE(`b`, false))) =>
      case _ => fail()
    }
    poss(ag) match {
      case Open(Nil) =>
      case _ => fail()
    }
    poss(p) match {
      case Open(Seq(PNE(`p`, false))) =>
      case _ => fail()
    }
    poss(a) match {
      case Open(Seq(PNE(`a`, false))) =>
      case _ => fail()
    }
  }
}
