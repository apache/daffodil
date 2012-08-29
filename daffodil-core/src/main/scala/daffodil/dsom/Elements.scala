package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import daffodil.processors.VariableMap
import daffodil.api.WithDiagnostics
import daffodil.dsom.OOLAG._
import daffodil.exceptions.ThrowsSDE
import daffodil.dsom.OOLAG.LV

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

// A Particle is something that can be repeating.
trait ParticleMixin { self : ElementBase =>

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
  lazy val hasStaticallyRequiredInstances = hasStaticallyRequiredInstances_.value
  private lazy val hasStaticallyRequiredInstances_ = LV {
    val res =
      if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true
      else false
    res
  }

  lazy val hasStopValue = hasStopValue_.value 
  private lazy val hasStopValue_ = LV {
    val sv = isRecurring && occursCountKind == OccursCountKind.StopValue
    // Don't check things like this aggressively. If we need occursStopValue then someone will ask for it.
    schemaDefinition(!(sv && occursStopValue == ""), "Property occursCountKind='stopValue' requires a non-empty occursStopValue property.")
    sv
  }
}

/**
 * Note about DSOM design versus say XSOM or Apache XSD library.
 *
 * Some XSD object models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances.
 *
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 */

/**
 * Shared by all forms of elements, local or global or element reference.
 */
abstract class ElementBase(xmlArg : Node, parent : SchemaComponent, position : Int)
  extends Term(xmlArg, parent, position)
  with AnnotatedMixin
  with Element_AnnotationMixin
  with NillableMixin
  with DFDLStatementMixin
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin
  with NamedMixin
  with WithDiagnostics {

  def inputValueCalcOption : Option[String]
  def isNillable : Boolean
  def isSimpleType : Boolean
  def isComplexType : Boolean
  def elementComplexType : ComplexTypeBase
  def elementSimpleType : SimpleTypeBase
  def typeDef : TypeBase
  def isScalar : Boolean
  
  override lazy val isRepresented = inputValueCalcOption == None

  def annotationFactory(node : Node) : DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLElement(newDFDLAnnotationXML("element"), this)

  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLElement]

  /**
   * Tells us if we have a specific length.
   *
   * Keep in mind that 80 characters in length can be anywhere from 80 to 320 bytes
   * depending on the character encoding. So fixed length doesn't mean in bytes.
   * it means in dfdl:lengthUnits units, which could be characters, and those can
   * be fixed or variable width.
   */
  lazy val isFixedLength = {
    lengthKind == LengthKind.Explicit && length.isConstant
  }

  lazy val fixedLength = {
    if (isFixedLength) length.constantAsLong else -1 // shouldn't even be asking for this if not isFixedLength 
  }

  // if it is of simple type, then facets like length, maxLength, minLength are
  // attributes of the simple type def. You can't put them directly on an element.

  /**
   * Nil Lit = literal nil, as opposed to value nil that uses a reserved value
   */
  lazy val isDefinedNilLit : Boolean = {
    val res = isNillable &&
      (nilKind == NilKind.LiteralValue ||
        nilKind == NilKind.LiteralCharacter)
    res
  }

  lazy val isDefinedNilValue : Boolean = {
    val res = (isNillable && nilKind == NilKind.LogicalValue)
    res
  }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */

  val NVDP = NilValueDelimiterPolicy
  val EVDP = EmptyValueDelimiterPolicy

  lazy val hasNilValueInitiator = initTermTestExpression(initiator, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  lazy val hasNilValueTerminator = initTermTestExpression(terminator, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  lazy val hasEmptyValueInitiator = initTermTestExpression(initiator, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  lazy val hasEmptyValueTerminator = initTermTestExpression(terminator, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any that is pass by name (aka lazy pass). 
  // That allows us to not require the property to exist at all if
  // expr.isKnownNotEmpty turns out to be false. 
  def initTermTestExpression(expr : CompiledExpression, prop : => Any, true1 : Any, true2 : Any) : Boolean = {
    // changed from a match on a 2-tuple to if-then-else logic because we don't even want to ask for 
    // prop's value at all unless the first test is true.
    if (expr.isKnownNonEmpty)
      if (prop == true1 || prop == true2) true
      else false
    else false
  }

  /**
   * Means the element is in a context where there is a separator (from some enclosing sequence)
   * expected after it.
   *
   * Abstract here because implementations are different for local vs. global things.
   */
  def hasSep : Boolean

  /**
   * check if there are delimiters such that there is a concept of something that we can call 'empty'
   */
  lazy val emptyIsAnObservableConcept = emptyIsAnObservableConcept_.value
  private lazy val emptyIsAnObservableConcept_ = LV {
    val res = if ((hasSep ||
      hasEmptyValueInitiator ||
      hasEmptyValueTerminator) &&
      lengthKind != LengthKind.Implicit) {
      // fixed length things can't be empty (assuming static length 0 isn't allowed.) 
      false
    } else true
    res
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

  /**
   * Does the element have a default value?
   */
  def isDefaultable : Boolean

}

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin
  extends ParticleMixin
  with LocalComponentMixin
  with LocalElementGrammarMixin { self : LocalElementBase =>

  lazy val hasSep = hasSep_.value
  lazy val hasSep_ = LV {
    nearestEnclosingSequence match {
      case None => false
      case Some(es) => {
        val res =
          es.separator.isKnownNonEmpty
        res
      }
    }
  }

  lazy val isDeclaredLastInSequence = LV {
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

  lazy val isLastRequiredElementOfSequence : Boolean = Assert.notYetImplemented()

  lazy val separatorSuppressionPolicy = separatorSuppressionPolicy_.value
  private lazy val separatorSuppressionPolicy_ = LV {
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

abstract class LocalElementBase(xmlArg : Node, parent : ModelGroup, position : Int)
extends ElementBase(xmlArg, parent, position)
with LocalElementMixin

class ElementRef(xmlArg : Node, parent : ModelGroup, position : Int)
  extends LocalElementBase(xmlArg, parent, position)
  with HasRef {
  
  // Need to go get the Element we are referencing
  lazy val referencedElement = referencedElement_.value // optionReferencedElement.get
//  lazy val optionReferencedElement = {
//    try referencedElement_.value
//    catch {
//      case e : ErrorAlreadyHandled => None
//    }
//  }
  
  private lazy val referencedElement_ = LV {
    this.schema.schemaSet.getGlobalElementDecl(namespace, localName) match {
      case None => SDE("Referenced element not found: %s.", this.ref)
      case Some(x) => x.forElementRef(this)
      }
  }

  // These will just delegate to the referenced element declaration
  lazy val isNillable = referencedElement.isNillable
  lazy val isSimpleType = referencedElement.isSimpleType
  lazy val isComplexType = referencedElement.isComplexType
  lazy val elementComplexType = referencedElement.elementComplexType 
  lazy val elementSimpleType = referencedElement.elementSimpleType
  lazy val isDefaultable : Boolean = referencedElement.isDefaultable

  lazy val qname = XMLUtils.QName(xml, xsdRef, schemaDocument)
  lazy val (namespace, localName) = qname
  override lazy val name = localName

  // These may be trickier, as the type needs to be responsive to properties from the
  // element reference's format annotations, and its lexical context.
  lazy val typeDef = referencedElement.typeDef//Assert.notYetImplemented()

  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.
  lazy val inputValueCalcOption = referencedElement.inputValueCalcOption // can't have ivc on element reference
  lazy val scalarDefaultable = referencedElement.scalarDefaultable
  /**
   * It is an error if the properties on an element overlap with the properties on the simple type
   * of that element.
   */
  lazy val overlappingProperties = {
    val referencedProperties = referencedElement.localAndFormatRefProperties.keySet
    val myLocalPropertiesNames = formatAnnotation.getFormatPropertiesNonDefault().keySet
    val intersect = referencedProperties.intersect(myLocalPropertiesNames)
    intersect
  }
  
  lazy val localAndFormatRefProperties = {
    val referencedProperties = referencedElement.localAndFormatRefProperties
    val myLocalProperties = this.formatAnnotation.getFormatPropertiesNonDefault()
    schemaDefinition(overlappingProperties.size == 0, "Element reference properties overlap with global element properties. Overlaps: %s.", overlappingProperties.mkString(" "))
    val theUnion = referencedProperties ++ myLocalProperties
    theUnion
    
  }

  lazy val diagnosticChildren = referencedElement_.toList  // optionReferencedElement.toList
}

/**
 * Element references and Group References use this.
 */
trait HasRef { self : SchemaComponent =>
  // TODO: Consolidate this and the xsdRef attributes that do QName stuff
  //From GroupRef.
  lazy val xsdRef = getAttributeRequired("ref")
  lazy val ref = xsdRef
}

/**
 * Shared by all element declarations local or global
 */
trait ElementDeclMixin
  extends NamedMixin
  with ElementDeclGrammarMixin { self : ElementBase =>

  lazy val immediateType = immediateType_.value
  private lazy val immediateType_ = LV {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = typeName
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), self))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), self))
    else {
      Assert.invariant(nt != "")
      None
    }
  }

  lazy val typeName = {
    val str = (xml \ "@type").text
    if (str == "") None else Some(str)
  }

  lazy val namedTypeQName = namedTypeQName_.value
  private lazy val namedTypeQName_ = LV {
    typeName match {
      case Some(tname) => Some(XMLUtils.QName(xml, tname, schemaDocument))
      case None => None
    }
  }

  lazy val namedTypeDef = namedTypeDef_.value
  private lazy val namedTypeDef_ = LV {
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
            // Note: Validation of the DFDL Schema doesn't necessarily check referential integrity
            // or other complex constraints like conflicting names.
            // So we check it here explicitly.
            case (None, None) => schemaDefinitionError("No type definition found for %s.", typeName.get)
            case (Some(_), Some(_)) => schemaDefinitionError("Both a simple and a complex type definition found for " + typeName.get + ".")
          }
          res
        }
      }
    }
  }

  lazy val typeDef = typeDef_.value
  private lazy val typeDef_ = LV {
    (immediateType, namedTypeDef) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case _ => SDE("Must have one of an immediate type or a named type but not both")
    }
  }

  lazy val elementDeclDiagnosticChildren = annotationObjs_.toList.flatten ++ typeDef_.toList
  
  lazy val isSimpleType = isSimpleType_.value
  private lazy val isSimpleType_ = LV {
    typeDef match {
      case _ : SimpleTypeBase => true
      case _ : ComplexTypeBase => false
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }
  
  lazy val isPrimitiveType = typeDef.isInstanceOf[PrimitiveType]

  lazy val isComplexType =  !isSimpleType 

  lazy val defaultValueAsString = (xml \ "@default").text

  lazy val hasDefaultValue : Boolean = defaultValueAsString != ""

  lazy val isNillable = (xml \ "@nillable").text == "true"

  lazy val elementComplexType = typeDef.asInstanceOf[ComplexTypeBase]
  lazy val elementSimpleType = typeDef.asInstanceOf[SimpleTypeBase]

  /**
   * We require that there be a concept of empty if we're going to be able to default something
   * and we are going to require that we can tell this statically. I.e., we're not going to defer this to runtime
   * just in case the delimiters are being determined at runtime.
   *
   * That is to say, if a delimiter is an expression, then we're assuming that means
   * at runtime it will not evaluate to empty string (so you can specify the delimiter
   * at runtime, but you cannot turn on/off the whole delimited format at runtime.)
   */

  lazy val isDefaultable = isDefaultable_.value
  private lazy val isDefaultable_ = LV {
    defaultValueAsString match {
      case "" => false // allowed for type string.
      case _ if (emptyIsAnObservableConcept) => true
      case _ => false
    }
  }

  lazy val localAndFormatRefProperties : Map[String, String] = {
    val localTypeProperties = typeDef.localAndFormatRefProperties
    val myLocalProperties = this.formatAnnotation.getFormatPropertiesNonDefault()
    schemaDefinition(overlappingProperties.size == 0, "Type properties overlap with element properties. Overlaps: %s.", overlappingProperties.mkString(" "))
    val theUnion = localTypeProperties ++ myLocalProperties
    theUnion
  }

  /**
   * It is an error if the properties on an element overlap with the properties on the simple type
   * of that element.
   */
  lazy val overlappingProperties = {
    val localTypePropertiesNames = typeDef.localAndFormatRefProperties.keySet
    val myLocalPropertiesNames = formatAnnotation.getFormatPropertiesNonDefault().keySet
    val intersect = localTypePropertiesNames.intersect(myLocalPropertiesNames)
    intersect
  }

  lazy val combinedElementAndSimpleTypeProperties = {
    var props : Map[String, String] = this.localAndFormatRefProperties

    if (isSimpleType && !isPrimitiveType ) {
      props ++= this.elementSimpleType.allNonDefaultProperties
    }
    props
  }

  override lazy val allNonDefaultProperties = {
    val theLocalUnion = this.combinedElementAndSimpleTypeProperties
    theLocalUnion
  }

}

class LocalElementDecl(xmlArg : Node, parent : ModelGroup, position : Int)
  extends LocalElementBase(xmlArg, parent, position)
  with ElementDeclMixin {
  // nothing here yet. All from the mixins.

  lazy val diagnosticChildren = elementDeclDiagnosticChildren
}

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can have
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait DFDLStatementMixin extends ThrowsSDE {

  def annotationFactoryForDFDLStatement(node : Node, self : AnnotatedSchemaComponent) : DFDLAnnotation = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => new DFDLAssert(node, self)
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => new DFDLDiscriminator(node, self)
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => new DFDLSetVariable(node, self)
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => new DFDLNewVariableInstance(node, self)
      case _ => SDE("Invalid DFDL annotation found: %s", node)
    }
  }

}

/**
 * Factory to create an instance of a global element declaration
 * either to be the root of the data, or when referenced from an
 * element reference, in which case a backpointer from the global element decl
 * instance will point back to the element reference.
 *
 * This backpointer is needed in order to determine some attributes that refer
 * outward to what something is contained within. E.g., nearestEnclosingSequence
 * is an attribute that might be the sequence containing the element reference
 * that is referencing this global element declaration.
 */
class GlobalElementDeclFactory(val xml : Node, val schemaDocument : SchemaDocument)
  extends NamedMixin {
  def forRoot() = asRoot // cache. Not a new one every time.
  lazy val asRoot = new GlobalElementDecl(xml, schemaDocument, None)

  def forElementRef(eRef : ElementRef) = new GlobalElementDecl(xml, schemaDocument, Some(eRef))

}

class GlobalElementDecl(xmlArg : Node, schemaDocumentArg : SchemaDocument, val elementRef : Option[ElementRef])
  extends ElementBase(xmlArg, schemaDocumentArg, 0)
  with ElementDeclMixin
  with GlobalComponentMixin
  with GlobalElementDeclGrammarMixin
  with WithDiagnostics {
  
  override val enclosingComponent = elementRef

  // We inherit the requirement for these attributes from Term. It all gets
  // too complicated in DSOM if you try to make GlobalElementDecl share with the other
  // element structures but not be a Term.
  //
  // But a GlobalElementDecl isn't really a Term except in a degenerate sense
  // that the root element is sort of a Term.
  //
  // In other words, we shouldn't be treating this as a term.
  //
  lazy val hasStaticallyRequiredInstances = Assert.impossible("Shouldn't call hasStaticallyRequiredInstances on a GlobalElementDecl")
  lazy val termContentBody = Assert.impossible("Shouldn't call termContentBody on a GlobalElementDecl")

  /**
   * global elements are always scalar. Element references referring to them can
   * be multiple occurring (aka arrays)
   */
  override lazy val isScalar = true

  lazy val hasSep = false // when a global decl is a root element then it's not in a sequence, and so can't be separated.
  // if this global decl is used via an element reference, then the element ref's definition of hasSep will be used, not this one.

  lazy val diagnosticChildren = elementDeclDiagnosticChildren :+ document

}

