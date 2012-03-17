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
  def getPropertyOption(pname: String) = formatAnnotation.getPropertyOption(pname) // delegate

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
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin {

  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def elementComplexType: ComplexTypeBase
  def elementSimpleType: SimpleTypeBase
  def typeDef: TypeBase
  
  
  lazy val compiledLength = expressionCompiler.compile('Long, length)
  
  lazy val isFixedLength = {
    lengthKind == LengthKind.Explicit && 
    length != null && // just check to insure length is defined.
    compiledLength.isConstant
  }
  
  def hasPrimitiveType(localname : String) : Boolean = {
    typeDef match {
      case prim : PrimitiveType => prim.name == localname
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

  lazy val hasInitiator = initiatorExpr.isKnownNonEmpty
  lazy val hasTerminator = terminatorExpr.isKnownNonEmpty
  
  lazy val hasNilValueInitiator = initTermTestExpression(initiatorExpr, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  lazy val hasNilValueTerminator = initTermTestExpression(terminatorExpr, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  lazy val hasEmptyValueInitiator = initTermTestExpression(initiatorExpr, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  lazy val hasEmptyValueTerminator = initTermTestExpression(terminatorExpr, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any. That allows us to not require the property to exist if
  // expr.isKnownNotEmpty turns out to be false. 
  def initTermTestExpression(expr: CompiledExpression, prop: => Any, true1: Any, true2: Any): Boolean = {
    (expr.isKnownNonEmpty, prop) match {
      case (true, _) => prop == true1 || prop == true2
      case _ => false
    }
  }

  /**
   * Means the element is in a context where there is a separator expected after it.
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
  
  def isDefaultable : Boolean

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

class ElementRef(xmlArg: Node, parent: ModelGroup)
  extends LocalElementBase(xmlArg, parent) with HasRef {

  // These will just delegate to the referenced element declaration
  lazy val isNillable = Assert.notYetImplemented()
  lazy val isSimpleType = Assert.notYetImplemented()
  lazy val isComplexType = Assert.notYetImplemented()
  lazy val elementComplexType: ComplexTypeBase = Assert.notYetImplemented()
  lazy val elementSimpleType: SimpleTypeBase = Assert.notYetImplemented()
  lazy val isDefaultable : Boolean = Assert.notYetImplemented()

  // These may be trickier, as the type needs to be responsive to properties from the
  // element reference's format annotations, and its lexical context.
  lazy val typeDef = Assert.notYetImplemented()
  override def grammarExpr = Assert.notYetImplemented()

  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.

}

trait HasRef { self: SchemaComponent =>
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

  lazy val typeName = {
    val str = (xml \ "@type").text
    if (str == "") None else Some(str)
  }

  /**
   * Translates a qualified name into a pair of a namespace uri, and a local name part.
   * 
   * Currently makes an effort to take unqualified names into the targetNamespace of the schema,
   */
  def QName(typeName: String): (String, String) = {
    val parts = typeName.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => ("", local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = xml.getNamespace(prefix) // should work even when there is no namespace prefix.
    // Assert.schemaDefinition(nsURI != null, "In QName " + typeName + ", the prefix " + prefix + " was not defined.")
    // TODO: accumulate errors, don't just throw on one.
    // TODO: error location for diagnostic purposes. 
    // see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
    
    // TODO: Clarify whether we should be tolerant this way, or strict
    val finalURI = if (nsURI == null || nsURI == "") schemaDocument.targetNamespace else nsURI
    (finalURI, localName)
  }

  lazy val namedTypeQName = {
    typeName match {
      case Some(tname) => Some(QName(tname))
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
            case (Some(_), None) => gstd
            case (None, Some(_)) => gctd
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

  val hasSep = false

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

