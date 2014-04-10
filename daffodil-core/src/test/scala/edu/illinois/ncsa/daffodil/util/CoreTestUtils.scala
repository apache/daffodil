package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.externalvars.Binding
import java.io.File
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.api.DFDL._
/**
 * We need a schema document and such for unit testing, also our PrimitiveType
 * needs a dummy schema document also so that our invariant, that *everything*
 * has a schema document, schema, and schema set
 * holds true even when we're not building up a "real" schema.
 */

object Fakes {
  def fakeDP = new Fakes().fakeDP
  def fakeElem = new Fakes().fakeElem
  def fakeSD = new Fakes().fakeSD
  def fakeGroupRef = new Fakes().fakeGroupRef
}

class Fakes private() {
  lazy val sch = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
    <xs:element name="fake2" type="tns:fakeCT"/>
    <xs:complexType name="fakeCT">
      <xs:sequence>
        <xs:group ref="tns:fakeGroup"/>
        <xs:element ref="tns:fake"/>
      </xs:sequence>
    </xs:complexType>
    <xs:group name="fakeGroup">
      <xs:choice>
        <xs:sequence/>
      </xs:choice>
    </xs:group>)
  val DummyPrimitiveFactory = null
  lazy val xsd_sset: SchemaSet = new SchemaSet(DummyPrimitiveFactory, sch, "http://example.com", "fake") 
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0) 
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get.forRoot()
  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.forRoot().typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.modelGroup.asInstanceOf[Sequence]
  lazy val Seq(fs1, fs2) = fakeSequence.groupMembers
  lazy val fakeGroupRef = fs1.asInstanceOf[GroupRef]

  class FakeDataProcessor extends DataProcessor {
    def setValidationMode(mode: ValidationMode.Type): Unit = {}
    def getValidationMode(): ValidationMode.Type = { ValidationMode.Full }
    def save(output: Output): Unit = {}
    def setExternalVariables(extVars: Map[String, String]): Unit = {}
    def setExternalVariables(extVars: Seq[Binding]): Unit = {}
    def setExternalVariables(extVars: File): Unit = {}
    def getVariables(): VariableMap = EmptyVariableMap
    def unparse(output: Output, node: scala.xml.Node): UnparseResult = null
    def parse(input: Input, lengthLimitInBits: Long = -1): ParseResult = null
    def parse(file: File) : ParseResult = null

    def getDiagnostics: Seq[Diagnostic] = Seq.empty
    //final lazy val canProceed: Boolean = !isError
    def isError: Boolean = false
  }

  lazy val fakeDP = new FakeDataProcessor

}