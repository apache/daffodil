package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props.gen._

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

/**
 * provides element-specific implementation of requirements from AnnotatedMixin
 */
trait AnnotatedElementMixin 
  extends AnnotatedMixin
  with Element_AnnotationMixin {
  def getPropertyOption(pname : String) = formatAnnotation.getPropertyOption(pname) // delegate
  
  def emptyFormatFactory = new DFDLElement(<dfdl:element/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]
}

// A Particle is something that can be repeating.
trait Particle { self: LocalElementBase =>

  lazy val isScalar = minOccurs == 1 && maxOccurs == 1
  lazy val isRecurring = !isScalar
  
  lazy val minOccurs = {
    val min = (self.xml \ "@minOccurs").text.toString
    min match {
      case "" => 1
      case _ => min.toInt
    }
  }

  lazy val maxOccurs = {
    val max = (self.xml \ "@maxOccurs").text.toString
    max match {
      case "unbounded" => -1
      case "" => 1
      case _ => max.toInt
    }
  }
  
  lazy val isFixedOccurrences = {
    // TODO optimizations to take scope into consideration. E.g.,
    // We could be in a context where the value of our occursCount expression
    // will always be a constant. 
    occursCountKind == OccursCountKind.Fixed
  }
  
  /**
   * Does this node have statically required instances.
   */
  lazy val hasStaticallyRequiredInstances: Boolean = {
    val res =
      if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true
      else false
    res
  }
}

/**
 * Some XSD models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances.
 *
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 */

/**
 * Shared by all forms of elements, local or global or element reference.
 */
trait ElementBaseMixin 
extends AnnotatedElementMixin
with ElementBaseGrammarMixin {
   
   def isNillable = (xml \ "@nillable").text == "true"
   def isSimpleType : Boolean
   def isComplexType : Boolean
   def elementComplexType : ComplexTypeBase = typeDef.asInstanceOf[ComplexTypeBase]
   def elementSimpleType : SimpleTypeBase = typeDef.asInstanceOf[SimpleTypeBase]
   def typeDef : TypeBase

}

abstract class LocalElementBase(xmlArg: Node, parent: ModelGroup)
  extends Term(xmlArg, parent)
  with ElementBaseMixin
  with Particle 
  with LocalElementBaseGrammarMixin {

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => super.annotationFactory(node)
    }
  }

}

class ElementRef(xmlArg: Node, parent: ModelGroup)
  extends LocalElementBase(xmlArg, parent) with HasRef {
  
  // These will just delegate to the referenced element declaration
   override def isNillable = Assert.notYetImplemented()
   override def isSimpleType = Assert.notYetImplemented()
   override def isComplexType = Assert.notYetImplemented()
   
   // These may be trickier, as the type needs to be responsive to properties from the
   // element reference's format annotations, and its lexical context.
   def typeDef = Assert.notYetImplemented()
   override def grammarExpr = Assert.notYetImplemented()
   
  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.
  
}

trait HasRef { self : SchemaComponent =>
  lazy val xsdRef = (xml \ "@ref").text
}

trait ElementDeclBase 
  extends ElementBaseMixin {
  
  lazy val immediateType: Option[TypeBase] = {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = typeName
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), this))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else {
      Assert.invariant(nt != "")
      None
    }
  }
  
  lazy val typeName = (xml \ "@type").text
  
  def QName(typeName : String) : (String, String) = {
    val parts = typeName.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => ("", local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = xml.getNamespace(prefix) // should work even when there is no namespace prefix.
    Assert.schemaDefinition(nsURI != null, "In QName " + typeName + ", the prefix " + prefix + " was not defined.")
    // TODO: accumulate errors, don't just throw on one.
    // TODO: error location for diagnostic purposes. 
    // see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
    (nsURI, localName)
  }
  
  lazy val namedTypeQName = QName(typeName)
  lazy val namedTypeDef : Option[TypeBase] = {
    val ss = schema.schemaSet
    val getGSTD = Function.tupled(ss.getGlobalSimpleTypeDef _)
    val getGCTD = Function.tupled(ss.getGlobalComplexTypeDef _)
    val gstd = getGSTD(namedTypeQName)
    val gctd = getGCTD(namedTypeQName)
    val res = (gstd, gctd) match {
      case (Some(_), None) => gstd
      case (None, Some(_)) => gctd
      case (None, None) => Assert.schemaDefinitionError("No type definition found for " + typeName + ".")
      // FIXME: do we need to do these checks, or has schema validation checked this for us?
      // FIXME: if we do have to check, then the usual problems: don't stop on first error, and need location of error in diagnostic.
      case (Some(_), Some(_)) => Assert.schemaDefinitionError("Both a simple and a complex type definition found for " + typeName + ".")
    }
    res
  }
  
  lazy val typeDef = {
    (immediateType, namedTypeDef) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Schema validation should find this for us, so this is not an SDE check. It's just an invariant.
      case _ => Assert.invariantFailed("Must have either an immediate type, or a named type, but not both")
    }
  }
  
  lazy val isSimpleType = {
    typeDef match {
      case _ : SimpleTypeBase => true
      case _ : ComplexTypeBase => false
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }
  
  lazy val isComplexType = !isSimpleType
  
}

class LocalElementDecl(xmlArg: Node, parent: ModelGroup)
  extends LocalElementBase(xmlArg, parent)
  with ElementDeclBase {    
}

trait DFDLStatementMixin {
  def annotationFactory(node: Node, self: AnnotatedMixin): DFDLAnnotation = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => new DFDLAssert(node, self)
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => new DFDLDiscriminator(node, self)
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => new DFDLSetVariable(node, self)
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => new DFDLNewVariableInstance(node, self)
      case _ => Assert.impossible("Invalid dfdl annotation found!")
    }
  }
}

class GlobalElementDecl(xmlArg: Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin
  with ElementDeclBase
  with DFDLStatementMixin 
  with GlobalElementDeclGrammarMixin {
  lazy val xml = xmlArg
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

