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

import org.junit.Test

class TestStreamingUnparserCompilerAttributes {

  import PossibleNextElements._

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
    val Seq(a: LE, b: LE) = rg.groupMembers
    val Closed(Seq(PNE(`r`, true))) = poss(r)
    val Closed(Nil) = poss(rg)
    val Closed(Seq(PNE(`a`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
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
    val Seq(a: LE, b: LE) = rg.groupMembers
    val Closed(Seq(PNE(`r`, true))) = poss(r)
    val Closed(Nil) = poss(rg)
    val Closed(Seq(PNE(`a`, false), PNE(`b`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
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
    val Seq(a: LE, b: LE) = rg.groupMembers
    val Closed(Seq(PNE(`r`, true))) = poss(r)
    val Closed(Nil) = poss(rg)
    val Closed(Seq(PNE(`a`, false), PNE(`b`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
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
    val Seq(a: LE, b: LE, gr: SGR) = rg.groupMembers
    val Seq(c: LE) = gr.groupMembers
    val Closed(Seq(PNE(`r`, true))) = poss(r)
    val Closed(Nil) = poss(rg)
    val Closed(Seq(PNE(`a`, false), PNE(`b`, false), PNE(`c`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, false), PNE(`c`, true))) = poss(b)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
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
    val Seq(a: LE, b: LE, gr: CGR) = rg.groupMembers
    val Seq(c: LE, d: LE) = gr.groupMembers
    val Closed(Seq(PNE(`a`, false), PNE(`b`, false), PNE(`c`, false), PNE(`d`, false))) = poss(
      a
    )
    val Closed(Seq(PNE(`b`, false), PNE(`c`, false), PNE(`d`, false))) = poss(b)
    val Closed(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(gr)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(sgr: SGR, cgr: CGR) = rg.groupMembers
    val Seq(a: LE, b: LE) = sgr.groupMembers
    val Seq(c: LE, d: LE) = cgr.groupMembers
    val Closed(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(sgr)
    val Open(Seq(PNE(`a`, false), PNE(`b`, false))) = poss(a)
    val Open(Seq(PNE(`b`, false))) = poss(b)
    val Closed(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(cgr)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE) = rg.groupMembers
    val Closed(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE, es: S) = rg.groupMembers
    val Open(Nil) = poss(es)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE, gr: SGR) = rg.groupMembers
    val Open(Nil) = poss(gr)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE, gr: SGR) = rg.groupMembers
    val Seq(e: LE) = gr.groupMembers
    val Open(Nil) = poss(gr)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE, gr: SGR) = rg.groupMembers
    val Seq(e: LE) = gr.groupMembers
    val Open(Nil) = poss(gr)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(c: LE, d: LE, gr: SGR) = rg.groupMembers
    val Seq(e: LE) = gr.groupMembers
    val Open(Nil) = poss(gr)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) = poss(rg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(s: S) = rg.groupMembers
    val Seq(cgr: CGR) = s.groupMembers
    val Seq(c: LE, d: LE, gr: SGR) = cgr.groupMembers
    val Seq(e: LE) = gr.groupMembers
    val Closed(Nil) = poss(rg)
    val Open(Nil) = poss(s)
    val Open(Seq(PNE(`c`, false), PNE(`d`, false), PNE(`e`, false))) = poss(cgr)
    val Open(Nil) = poss(gr)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Closed(Seq(PNE(`d`, true))) = poss(d)
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
    val Seq(a: LE, b: LE, c: LE) = rg.groupMembers
    val Closed(Seq(PNE(`a`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, false), PNE(`c`, true))) = poss(b)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
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
    val Seq(g2c1: LE, g2c2: LE, g2c3: LE, hg: CGR, vg: CGR) = rg.groupMembers
    val Closed(Seq(PNE(`g2c1`, true))) = poss(g2c1)
    val Closed(Seq(PNE(`g2c2`, true))) = poss(g2c2)
    val Closed(Seq(PNE(`g2c3`, false), PNE(vg_inty, false), PNE(vg_stringy, false))) = poss(
      g2c3
    )
    val Open(Seq(PNE(`vg_inty`, false), PNE(`vg_stringy`, false))) = poss(hg)
    val Closed(Seq(PNE(`vg_inty`, false), PNE(`vg_stringy`, false))) = poss(vg)
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
    val Seq(a: LE, hgr: SGR, b: LE) = rg.groupMembers
    val Seq(e: LE) = hgr.groupMembers
    val Closed(Seq(PNE(`a`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
    val Closed(Seq(PNE(`b`, true))) = poss(hgr)
    val Open(Seq(PNE(`e`, false))) = poss(e)
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
    val Seq(a: LE, cg: C, c: LE) = rg.groupMembers
    val Seq(s1: S, s2: SGR) = cg.groupMembers
    val Seq(pg: SGR, b: LE) = s1.groupMembers
    val Seq(aflag: LE) = s2.groupMembers
    val Seq(pflag: LE) = pg.groupMembers
    val Closed(Seq(PNE(`a`, true))) = poss(a)
    val Closed(Seq(PNE(`b`, false), PNE(`c`, true))) = poss(cg)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Open(Nil) = poss(s1)
    val Open(Nil) = poss(s2)
    val Closed(Seq(PNE(`b`, true))) = poss(pg)
    val Open(Seq(PNE(`pflag`, false))) = poss(pflag)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
    val Open(Seq(PNE(`aflag`, false))) = poss(aflag)
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
    val Seq(a: LE, cg1: C, cg2: C, d: LE) = rg.groupMembers
    val Seq(s1a: S, s1b: SGR) = cg1.groupMembers
    val Seq(pg1: SGR, b: LE) = s1a.groupMembers
    val Seq(aflag1: LE) = s1b.groupMembers
    val Seq(pflag1: LE) = pg1.groupMembers

    val Seq(s2a: S, s2b: SGR) = cg2.groupMembers
    val Seq(pg2: SGR, c: LE) = s2a.groupMembers
    val Seq(aflag2: LE) = s2b.groupMembers
    val Seq(pflag2: LE) = pg2.groupMembers

    val Closed(Seq(PNE(`a`, true))) = poss(a)

    val Closed(Seq(PNE(`b`, false), PNE(`c`, false), PNE(`d`, true))) = poss(cg1)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Open(Nil) = poss(s1a)
    val Open(Nil) = poss(s1b)
    val Closed(Seq(PNE(`b`, true))) = poss(pg1)
    val Open(Seq(PNE(`pflag1`, false))) = poss(pflag1)
    val Closed(Seq(PNE(`b`, true))) = poss(b)
    val Open(Seq(PNE(`aflag1`, false))) = poss(aflag1)

    val Closed(Seq(PNE(`c`, false), PNE(`d`, true))) = poss(cg2)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Open(Nil) = poss(s2a)
    val Open(Nil) = poss(s2b)
    val Closed(Seq(PNE(`c`, true))) = poss(pg2)
    val Open(Seq(PNE(`pflag2`, false))) = poss(pflag2)
    val Closed(Seq(PNE(`c`, true))) = poss(c)
    val Open(Seq(PNE(`aflag2`, false))) = poss(aflag2)
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
    val Seq(ch: C, y: LE) = rg.groupMembers
    val Seq(s1: S, ag: SGR) = ch.groupMembers
    val Seq(pg: SGR, b: LE) = s1.groupMembers
    val Seq(p: LE) = pg.groupMembers
    val Seq(a: LE) = ag.groupMembers

    val Open(Seq(PNE(`b`, false), PNE(`y`, false))) = poss(ch)
    val Open(Nil) = poss(s1)
    val Open(Seq(PNE(`y`, false))) = poss(y)
    val Open(Seq(PNE(`b`, false))) = poss(pg)
    val Open(Nil) = poss(ag)
    val Open(Seq(PNE(`p`, false))) = poss(p)
    val Open(Seq(PNE(`a`, false))) = poss(a)
  }
}
