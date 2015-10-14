package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.compiler._
import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.processors.unparsers.GeneralAugmenter
import edu.illinois.ncsa.daffodil.processors.unparsers.OutputValueCalcAugmenter
import edu.illinois.ncsa.daffodil.equality._

class TestUnparserAttributes {

  def compileToRoot(testSchema: scala.xml.Elem): GlobalElementDecl = {
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    root
  }

  @Test def testElementPossibilityTree_1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "ovc" }' dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(ovc: LocalElementBase) = rootSeq.groupMembers

    val Leaf(ovcEB: LocalElementBase) = ovc.elementPossibilityTree
    assertEquals(ovc, ovcEB)

  }

  @Test def testElementPossibilityTree_2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" occursCountKind="implicit"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="opt" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "ovc" }' dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(opt: LocalElementBase, ovc: LocalElementBase) = rootSeq.groupMembers

    assertTrue(opt.isOptional)
    val Ord(Seq(Leaf(opt1), Leaf(ovc1))) = root.sequence.elementPossibilityTree
    assertEquals(ovc, ovc1)
    assertEquals(opt, opt1)

  }

  @Test def testFollowingElementTree_1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" occursCountKind="implicit"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="opt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="opt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "ovc" }' dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(opt1: LocalElementBase, opt2: LocalElementBase, ovc1: LocalElementBase) = rootSeq.groupMembers

    val ept = root.sequence.elementPossibilityTree
    val fet = root.sequence.followingElementTree
    val Ord(Seq(Leaf(opt1_), Leaf(opt2_), Leaf(ovc1_))) = ept

    val Ord(Seq(Cho(Seq(Leaf(l1), Leaf(_))), Leaf(_))) = fet
    assertNotNull(l1)

    assertEquals(ovc1, ovc1_)
    assertEquals(opt1_, opt1)
    assertEquals(opt2_, opt2)

    val sis @ Seq() = ovc1.siblingIndicators.map { _.namedQName.local }
    assertNotNull(sis)

  }

  @Test def testFollowingElementTree_2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" occursCountKind="implicit"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="opt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="opt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "ovc" }' dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="postOpt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="postOpt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(opt1: LocalElementBase, opt2: LocalElementBase, ovc1: LocalElementBase, postOpt1, postOpt2) = rootSeq.groupMembers

    val ept = root.sequence.elementPossibilityTree
    val fet = root.sequence.followingElementTree
    val eptPat @ Ord(Seq(Leaf(`opt1`), Leaf(`opt2`), Leaf(`ovc1`), Leaf(`postOpt1`), Leaf(`postOpt2`))) = ept
    assertNotNull(eptPat)

    val fetPat @ Ord(Seq(Cho(Seq(Leaf(`opt1`), Leaf(`opt2`))), Leaf(`ovc1`), Cho(Seq(Leaf(`postOpt1`), Leaf(`postOpt2`))))) = fet
    assertNotNull(fetPat)

    val sis @ Seq("postOpt1", "postOpt2") = ovc1.siblingIndicators.map { _.namedQName.local }
    assertNotNull(sis)

  }

  /**
   * This test is like prior, except wraps lots of spurious xs:sequence and xs:choice
   * around things. The summarized trees should stay the same.
   */
  @Test def testFollowingElementTree_4() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" occursCountKind="implicit"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:element name="opt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              <xs:sequence>
                <xs:element name="opt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              </xs:sequence>
            </xs:choice>
            <xs:choice>
              <xs:sequence>
                <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "ovc" }' dfdl:lengthKind="explicit" dfdl:length="3"/>
              </xs:sequence>
            </xs:choice>
            <xs:sequence>
              <xs:sequence>
                <xs:sequence>
                  <xs:element name="postOpt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
                </xs:sequence>
                <xs:element name="postOpt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              </xs:sequence>
              <xs:sequence/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(choice1: Choice, choice2: Choice, seq1: Sequence) = rootSeq.groupMembers
    val Seq(opt1: LocalElementBase, seq2: Sequence) = choice1.groupMembers
    val Seq(opt2: LocalElementBase) = seq2.groupMembers
    val Seq(seq3: Sequence) = choice2.groupMembers
    val Seq(ovc1: LocalElementBase) = seq3.groupMembers
    val Seq(seq4: Sequence, seq5: Sequence) = seq1.groupMembers
    val Seq(seq6: Sequence, postOpt2: LocalElementBase) = seq4.groupMembers
    val gms @ Seq() = seq5.groupMembers
    assertNotNull(gms)
    val Seq(postOpt1: LocalElementBase) = seq6.groupMembers

    val ept = root.sequence.elementPossibilityTree
    val fet = root.sequence.followingElementTree
    val eptPat @ Ord(Seq(Cho(Seq(Leaf(`opt1`), Leaf(`opt2`))), Leaf(`ovc1`), Leaf(`postOpt1`), Leaf(`postOpt2`))) = ept
    assertNotNull(eptPat)

    val fetPat @ Ord(Seq(Cho(Seq(Leaf(`opt1`), Leaf(`opt2`))), Leaf(`ovc1`), Cho(Seq(Leaf(`postOpt1`), Leaf(`postOpt2`))))) = fet
    assertNotNull(fetPat)

    val sis @ Seq("postOpt1", "postOpt2") = ovc1.siblingIndicators.map { _.namedQName.local }
    assertNotNull(sis)

  }

  @Test def testFollowingElementTree_5() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" occursCountKind="implicit"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:element name="opt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              <xs:sequence>
                <xs:element name="opt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              </xs:sequence>
            </xs:choice>
            <xs:choice>
              <xs:sequence>
                <xs:choice>
                  <!-- ambiguous. So we should always get ovc1 -->
                  <xs:element name="ovc1" type="xs:string" dfdl:outputValueCalc='{ "ovc1" }' dfdl:lengthKind="explicit" dfdl:length="4"/>
                  <xs:element name="ovc2" type="xs:string" dfdl:outputValueCalc='{ "ovc2" }' dfdl:lengthKind="explicit" dfdl:length="4"/>
                </xs:choice>
              </xs:sequence>
            </xs:choice>
            <xs:sequence>
              <xs:sequence>
                <xs:sequence>
                  <xs:element name="postOpt1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
                </xs:sequence>
                <xs:element name="postOpt2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="3"/>
              </xs:sequence>
              <xs:sequence/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(testSchema)
    val rootSeq = root.sequence
    val Seq(choice1: Choice, choice2: Choice, seq1: Sequence) = rootSeq.groupMembers
    val Seq(opt1: LocalElementBase, seq2: Sequence) = choice1.groupMembers
    val Seq(opt2: LocalElementBase) = seq2.groupMembers
    val Seq(seq3: Sequence) = choice2.groupMembers
    val Seq(choice3: Choice) = seq3.groupMembers
    val Seq(ovc1: LocalElementBase, ovc2: LocalElementBase) = choice3.groupMembers
    val Seq(seq4: Sequence, seq5: Sequence) = seq1.groupMembers
    val Seq(seq6: Sequence, postOpt2: LocalElementBase) = seq4.groupMembers
    val gms @ Seq() = seq5.groupMembers
    assertNotNull(gms)
    val Seq(postOpt1: LocalElementBase) = seq6.groupMembers

    val ept = root.sequence.elementPossibilityTree
    val fet = root.sequence.followingElementTree

    val eptPat @ Ord(Seq(Cho(Seq(Leaf(`opt1`), Leaf(`opt2`))), Cho(Seq(Leaf(`ovc1`), Leaf(`ovc2`))), Leaf(`postOpt1`), Leaf(`postOpt2`))) = ept
    assertNotNull(eptPat)

    val fetPat @ Ord(Seq(Cho(Seq(Leaf(`opt1`), Leaf(`opt2`))), Cho(Seq(Leaf(`ovc1`), Leaf(`ovc2`))), Cho(Seq(Leaf(`postOpt1`), Leaf(`postOpt2`))))) = fet
    assertNotNull(fetPat)

    val sis1 @ Seq("postOpt1", "postOpt2") = ovc1.siblingIndicators.map { _.namedQName.local }
    assertNotNull(sis1)

    val sis2 @ Seq("postOpt1", "postOpt2") = ovc2.siblingIndicators.map { _.namedQName.local }
    assertNotNull(sis2)

    val pfciPat @ Seq(`ovc1`, `ovc2`) = root.possibleFirstChildInsertables
    assertNotNull(pfciPat)

    val augMap = root.maybeChildInfosetAugmenter.get.asInstanceOf[GeneralAugmenter].augmenterMap

    val Seq((nqn1, aug1: OutputValueCalcAugmenter), (nqn2, aug2: OutputValueCalcAugmenter)) = augMap.toList
    assertEquals("postOpt2", nqn1.local)
    assertEquals("postOpt1", nqn2.local)

    // So the above schema is ambiguous. There are two OVCs alone in a choice.
    // That means only the first one will ever be unparsed, so all these are ovc1.
    //
    assertTrue(ovc1.erd =:= aug1.erd)
    assertTrue(ovc1.erd =:= aug2.erd)

  }

  @Test def testUnparserOVCWithRequiredAfter1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="before" type="xs:string" dfdl:initiator="beforeFoo" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="ovc" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="after" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(sch)
    val Ord(Seq(Leaf(_), Leaf(ovc), Leaf(after))) = root.childrenFollowingElementTree
    val pfci @ Seq(`ovc`) = root.possibleFirstChildInsertables; assertNotNull(pfci)
    val sis @ Seq(`after`) = ovc.siblingIndicators; assertNotNull(sis)
  }

  @Test def testUnparserDefaultable1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!-- need a terminator below. Otherwise we cannot distinguish empty from missing -->
            <xs:element name="defaultable" type="xs:string" dfdl:lengthKind="delimited" default="hello" dfdl:terminator="."/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(sch)
    val Ord(Seq(Leaf(defaultable))) = root.childrenFollowingElementTree
    val pfci @ Seq(`defaultable`) = root.possibleFirstChildInsertables; assertNotNull(pfci)
    val sis @ Seq() = defaultable.siblingIndicators; assertNotNull(sis)
  }

  @Test def testUnparserDefaultable2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!-- need a terminator below. Otherwise we cannot distinguish empty from missing -->
            <xs:element name="defaultable" type="xs:string" dfdl:lengthKind="delimited" default="hello" dfdl:terminator="."/>
            <xs:element name="after" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(sch)
    val Ord(Seq(Leaf(defaultable), Leaf(after))) = root.childrenFollowingElementTree
    val pfci @ Seq(`defaultable`) = root.possibleFirstChildInsertables; assertNotNull(pfci)
    val sis @ Seq(`after`) = defaultable.siblingIndicators; assertNotNull(sis)
  }

  @Test def testUnparserDefaultable3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" xmlns:ex={ XMLUtils.EXAMPLE_NAMESPACE }>
        <xs:complexType>
          <xs:sequence>
            <xs:element name="before1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ ../ex:before1 }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val root = compileToRoot(sch)
    val Ord(Seq(Leaf(before1), Leaf(ovc1))) = root.childrenFollowingElementTree
    val pfci1 @ Seq() = root.possibleFirstChildInsertables; assertNotNull(pfci1)
    val pfci2 @ Seq(`ovc1`) = before1.possibleFirstSiblingInsertables; assertNotNull(pfci2)
    val maybeRootCAug = root.maybeChildInfosetAugmenter
    assertTrue(maybeRootCAug.isDefined)
    assertTrue(before1.maybeChildInfosetAugmenter.isEmpty)
    assertTrue(before1.maybeLaterSiblingInfosetAugmenter.isDefined)
    val pfci @ Seq(`ovc1`) = before1.possibleFirstSiblingInsertables; assertNotNull(pfci)
    val sis @ Seq() = ovc1.siblingIndicators; assertNotNull(sis)
  }
}
