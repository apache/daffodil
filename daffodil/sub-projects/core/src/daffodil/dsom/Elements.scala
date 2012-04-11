package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import daffodil.processors.VariableMap

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

/**
 * provides element-specific implementation of requirements from AnnotatedMixin
 */
trait AnnotatedElementMixin
  extends AnnotatedMixin
  with Element_AnnotationMixin {

  def emptyFormatFactory = new DFDLElement(<dfdl:element/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]
}

// A Particle is something that can be repeating.
trait Particle { self: LocalElementBase =>

  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1
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

  lazy val hasStopValue = {
    val sv = isRecurring && occursCountKind == OccursCountKind.StopValue
    // Don't check things like this aggressively. If we need occursStopValue then someone will ask for it.
    Assert.schemaDefinition(!(sv && occursStopValue == ""), "Property occursCountKind='stopValue' requires a non-empty occursStopValue property.")
    sv
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
  with DFDLStatementMixin
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin
  with NamedMixin {

  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def elementComplexType: ComplexTypeBase
  def elementSimpleType: SimpleTypeBase
  def typeDef: TypeBase
  def isScalar: Boolean

  lazy val isFixedLength = {
    lengthKind == LengthKind.Explicit && length.isConstant
  }

  lazy val fixedLength = {
    if (isFixedLength) length.constantAsLong else -1 // shouldn't even be asking for this if not isFixedLength 
  }

  def hasPrimitiveType(localname: String): Boolean = {
    typeDef match {
      case prim: PrimitiveType => prim.name == localname
      case _ => false
    }
  }

  // if it is of simple type, then facets like length, maxLength, minLength are
  // attributes of the simple type def. You can't put them directly on an element.

  lazy val isDefinedNilLit: Boolean = {
    val res = isNillable &&
      (nilKind == NilKind.LiteralValue ||
        nilKind == NilKind.LiteralCharacter)
    res
  }

  lazy val isDefinedNilValue: Boolean = {
    val res = (isNillable && nilKind == NilKind.LogicalValue)
    res
  }

  
  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */

  val NVDP = NilValueDelimiterPolicy
  val EVDP = EmptyValueDelimiterPolicy

  lazy val hasNilValueInitiator = initTermTestExpression(initiatorExpr, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  lazy val hasNilValueTerminator = initTermTestExpression(terminatorExpr, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  lazy val hasEmptyValueInitiator = initTermTestExpression(initiatorExpr, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  lazy val hasEmptyValueTerminator = initTermTestExpression(terminatorExpr, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any. That allows us to not require the property to exist at all if
  // expr.isKnownNotEmpty turns out to be false. 
  def initTermTestExpression(expr: CompiledExpression, prop: => Any, true1: Any, true2: Any): Boolean = {
    // changed from a match on a 2-tuple to if-then-else logic because we don't even want to ask for 
    // prop's value at all unless the first test is true.
    if (expr.isKnownNonEmpty)
      if (prop == true1 || prop == true2) true
      else false
    else false
  }

  /**
   * Means the element is in a context where there is a separator expected after it.
   *
   * Abstract here because implementations are different for localElement
   */
  def hasSep: Boolean

  /**
   * check if there are delimiters such that there is a concept of something that we can call 'empty'
   */
  lazy val emptyIsAnObservableConcept: Boolean = {
    if ((hasSep ||
      hasEmptyValueInitiator ||
      hasEmptyValueTerminator) &&
      lengthKind != LengthKind.Implicit) {
      // fixed length things can't be empty (assuming static length 0 isn't allowed.) 
      false
    } else true
  }

  /**
   * everything that we need to look for when deciding how to terminate a data region
   * based on scanning
   */
  lazy val inScopeTerminatingMarkup = {
    // our own terminator is one thing
    // the separator of an enclosing group, if we're not last.
    // the terminator of an enclosing group, if we are last
    // the terminator of an enclosing element
    // recursively outward.
    //
    // or another way to think of it is
    // a sequence member has terminating markup, which is its separator for any item but the last, (last too if postfix), and the sequence terminator for the
    // last member. Plus any inscope terminating markup from what it is encapsulated in.
    // 
    // an element has its terminator
    //
    // Note: if we are potentially the last item (not required, but no downstream required siblings)
    Assert.notYetImplemented()
  }

  def isDefaultable: Boolean

}

abstract class LocalElementBase(xmlArg: Node, parent: ModelGroup, position: Int)
  extends Term(xmlArg, parent, position)
  with ElementBaseMixin
  with Particle
  with LocalElementBaseGrammarMixin {

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  lazy val hasSep = {
    nearestEnclosingSequence match {
      case None => false
      case Some(es) => {
        val res =
          es.separatorExpr.isKnownNonEmpty
        res
      }
    }
  }

  lazy val isDeclaredLastInSequence: Boolean = {
    val es = nearestEnclosingSequence
    // how do we determine what child node we are? We search. 
    // TODO: better structure for O(1) answer to this.
    es match {
      case None => true
      case Some(s) =>
        if (s.groupMembers.last eq this) true // we want object identity comparison here, not equality. 
        else false
    }
  }

  lazy val isLastRequiredElementOfSequence: Boolean = Assert.notYetImplemented()

  lazy val separatorSuppressionPolicy = {
    nearestEnclosingSequence match {
      case Some(ssp) => ssp.separatorSuppressionPolicy
      //
      // There is no enclosing sequence (could be just a big nest of choices I suppose)
      // In that case, there still has to be a value for this.
      //
      // Strictly speaking, it's a bug to require this property in a situation where there
      // are no sequences at all. However, that's so unlikely to be the case that it's 
      // not worth paying attention to. In any real format, the root element will be
      // a sequence.
      //
      case None => schemaDocument.separatorSuppressionPolicy
    }
  }  
}

class ElementRef(xmlArg: Node, parent: ModelGroup, position: Int)
  extends LocalElementBase(xmlArg, parent, position) with HasRef {

  // These will just delegate to the referenced element declaration
  lazy val isNillable = Assert.notYetImplemented()
  lazy val isSimpleType = Assert.notYetImplemented()
  lazy val isComplexType = Assert.notYetImplemented()
  lazy val elementComplexType: ComplexTypeBase = Assert.notYetImplemented()
  lazy val elementSimpleType: SimpleTypeBase = Assert.notYetImplemented()
  lazy val isDefaultable: Boolean = Assert.notYetImplemented()

  lazy val qname = XMLUtil.QName(xml, xsdRef, schemaDocument)
  override lazy val (namespace, name) = qname

  // These may be trickier, as the type needs to be responsive to properties from the
  // element reference's format annotations, and its lexical context.
  lazy val typeDef = Assert.notYetImplemented()

  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.

  lazy val localAndRefProperties = Assert.notYetImplemented()
}

trait HasRef { self: SchemaComponent =>
  lazy val xsdRef = getAttributeRequired("ref")
  lazy val ref = xsdRef
}

trait ElementDeclBase
  extends ElementBaseMixin
  with ElementDeclGrammarMixin {

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

  lazy val typeName = {
    val str = (xml \ "@type").text
    if (str == "") None else Some(str)
  }

  lazy val namedTypeQName = {
    typeName match {
      case Some(tname) => Some(XMLUtil.QName(xml, tname, schemaDocument))
      case None => None
    }
  }

  lazy val namedTypeDef: Option[TypeBase] = {
    namedTypeQName match {
      case None => None
      case Some((ns, localpart)) => {

        val ss = schema.schemaSet
        val prim = ss.getPrimitiveType(localpart)
        //
        if (prim != None) prim
        else {
          val gstd = ss.getGlobalSimpleTypeDef(ns, localpart)
          val gctd = ss.getGlobalComplexTypeDef(ns, localpart)
          val res = (gstd, gctd) match {
            case (Some(gstdFactory), None) => Some(gstdFactory.forElement(this))
            case (None, Some(gctdFactory)) => Some(gctdFactory.forElement(this))
            case (None, None) => Assert.schemaDefinitionError("No type definition found for " + typeName + ".")
            // FIXME: do we need to do these checks, or has schema validation checked this for us?
            // FIXME: if we do have to check, then the usual problems: don't stop on first error, and need location of error in diagnostic.
            case (Some(_), Some(_)) => Assert.schemaDefinitionError("Both a simple and a complex type definition found for " + typeName + ".")
          }
          res
        }
      }
    }
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
      case _: SimpleTypeBase => true
      case _: ComplexTypeBase => false
      case _: PrimitiveType => true
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }

  lazy val isComplexType = !isSimpleType

  lazy val defaultValueAsString = (xml \ "@default").text

  lazy val hasDefaultValue: Boolean = defaultValueAsString != ""

  lazy val isNillable = (xml \ "@nillable").text == "true"

  lazy val elementComplexType: ComplexTypeBase = typeDef.asInstanceOf[ComplexTypeBase]
  lazy val elementSimpleType: SimpleTypeBase = typeDef.asInstanceOf[SimpleTypeBase]

  /**
   * We require that there be a concept of empty if we're going to be able to default something
   * and we are going to require that we can tell this statically. I.e., we're not going to defer this to runtime
   * just in case the delimiters are being determined at runtime.
   *
   * That is to say, if a delimiter is an expression, then we're assuming that means
   * at runtime it will not evaluate to empty string (so you can specify the delimiter
   * at runtime, but you cannot turn on/off the whole delimited format at runtime.)
   */

  lazy val isDefaultable = {
    defaultValueAsString match {
      case "" => false // allowed for type string.
      case _ if (emptyIsAnObservableConcept) => true
      case _ => false
    }
  }

  lazy val localAndRefProperties: Map[String, String] = {
    val localTypeProperties = this.typeDef.localAndRefProperties
    val myLocalProperties = this.formatAnnotation.getFormatPropertiesNonDefault()
    Assert.schemaDefinition(overlappingProperties.size == 0, "Type properties overlap with element properties.")
    val theUnion = localTypeProperties ++ myLocalProperties
    theUnion
  }

  lazy val overlappingProperties = {
    val localTypePropertiesNames = this.typeDef.localAndRefProperties.map(x => x._1).toSet
    val myLocalPropertiesNames = this.formatAnnotation.getFormatPropertiesNonDefault().map(x => x._1).toSet
    val intersect = localTypePropertiesNames.intersect(myLocalPropertiesNames)
    intersect
  }
}

class LocalElementDecl(xmlArg: Node, parent: ModelGroup, position: Int)
  extends LocalElementBase(xmlArg, parent, position)
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

class GlobalElementDeclFactory(xmlArg: Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin {
  def xml = xmlArg

  def forRoot() = new GlobalElementDecl(xmlArg, schemaDocument, None)

  def forElementRef(eRef: ElementRef) = new GlobalElementDecl(xmlArg, schemaDocument, Some(eRef))

}

class GlobalElementDecl(xmlArg: Node, val schemaDocument: SchemaDocument, val elementRef: Option[ElementRef])
  extends GlobalComponentMixin
  with ElementDeclBase
  with GlobalElementDeclGrammarMixin {

  lazy val xml = xmlArg
  lazy val isScalar = true

  val hasSep = false // when a global decl is a root element then it's not in a sequence, and so can't be separated.
  // if this global decl is used via an element reference, then the element ref's definition of hasSep will be used, not this one.

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

