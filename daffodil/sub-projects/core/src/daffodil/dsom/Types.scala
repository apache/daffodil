package daffodil.dsom

import scala.xml._
import daffodil.exceptions._
import daffodil.grammar._
import daffodil.xml._

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase

trait NamedType extends NamedMixin with TypeBase with SchemaComponent

abstract class SimpleTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends TypeBase with AnnotatedMixin with DFDLStatementMixin {
  lazy val xml = xmlArg

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

abstract class NamedSimpleTypeBase(xmlArg: => Node, parent: => SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent) with NamedType {
}

class LocalSimpleTypeDef(xmlArg: Node, parent: SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent)
  with LocalComponentMixin {

  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  lazy val base = (xml \ "restriction" \ "@base").text

}

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) extends NamedType {
  //
  // Lots of faking & dummy objects here
  //
  override lazy val name = name_ 
  override lazy val namespace = XMLUtil.XSD_NAMESPACE
  lazy val xml = Assert.invariantFailed("Primitives don't have xml definitions.")
  lazy val dummySchemaSet = new SchemaSet(NodeSeq.Empty)
  lazy val xsdSchema = new Schema(namespace, NodeSeq.Empty, dummySchemaSet)
  lazy val schemaDocument = new SchemaDocument(<schema/>, xsdSchema)
}

class GlobalSimpleTypeDef(xmlArg: Node, val schemaDocument: SchemaDocument)
  extends NamedSimpleTypeBase(xmlArg, schemaDocument) with GlobalComponentMixin {

  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)

  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponent
  with TypeBase
  with ComplexTypeBaseGrammarMixin {

  lazy val xml = xmlArg
  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap { ModelGroup(_, this) }

}

class GlobalComplexTypeDef(xmlArg: Node, val schemaDocument: SchemaDocument)
  extends ComplexTypeBase(xmlArg, schemaDocument)
  with GlobalComponentMixin {
}

class LocalComplexTypeDef(xmlArg: Node, parent: SchemaComponent)
  extends ComplexTypeBase(xmlArg, parent)
  with LocalComponentMixin {
}
