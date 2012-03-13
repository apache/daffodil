package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml.XMLUtil
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
/*
 * Not using XSOM - too many issues. Schema Documents aren't first class objects, and we need
 * them to implement lexical scoping of default formats over the contents of a schema document.
 * 
 * import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
 * import com.sun.xml.xsom._
 * import com.sun.xml.xsom.util._
 */
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
//import parser.AnnotationParser

import daffodil.grammar._

trait SchemaComponent {
  def schemaDocument: SchemaDocument
  lazy val schema : Schema = schemaDocument.schema
  def xml : Node

  val NYI = false // our flag for Not Yet Implemented 
  
  lazy val expressionCompiler = new ExpressionCompiler(this)
}

trait LocalComponentMixin
  extends SchemaComponent {
  def parent: SchemaComponent
  lazy val schemaDocument = parent.schemaDocument
}

/**
 * Anything named has a name from its name attribute and a namespace from the
 * schema document it is part of.
 */
trait NamedMixin { self: SchemaComponent =>
  lazy val name = (xml \ "@name").text
  lazy val namespace = self.schemaDocument.targetNamespace
}

trait GlobalComponentMixin
  extends SchemaComponent
  with NamedMixin

trait AnnotatedMixin extends SchemaComponent {
  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  def annotationFactory(node: Node): DFDLAnnotation

  lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  /**
   * dais = Dfdl App Info nodeSeq
   */
  lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => false
          case Some(n) => {
            val str = n.text
            val hasRightSource = (str == "http://www.ogf.org/dfdl/dfdl-1.0/")
            val isAcceptable = str.startsWith("http://www.ogf.org/dfdl")
            (hasRightSource || isAcceptable) //TODO: remove lax check once examples & tests are updated.
          }
        }
      }
    }
    dais
  }

  lazy val annotationObjs = {
    dais.flatMap { dai =>
      {
        val children = dai.child
        val res = children.filterNot { _.isInstanceOf[Text] }.map { child =>
          {
            annotationFactory(child)
          }
        }
        res
      }
    }
  }

  /**
   * Here we establish an invariant which is that every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can
   * delegate various property-related attribute calculations to it.
   * 
   * To realize this, every concrete class must implement (or inherit) an implementation of 
   * emptyFormatFactory, which constructs an empty format annotation,
   * and isMyAnnotation which tests if an annotation is the corresponding kind.
   * 
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  def emptyFormatFactory: DFDLAnnotation
  def isMyAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation = {
    val format = annotationObjs.find { isMyAnnotation(_) }
    val res = format match {
      case None => emptyFormatFactory
      case Some(x) => x
    }
    res
  }
}

abstract class Annotated(xmlArg: Node)
  extends SchemaComponent with AnnotatedMixin {
  lazy val xml = xmlArg
}

/**
 * A schema set is exactly that, a set of schemas. Each schema has
 * a target namespace, so a schema set is conceptually a mapping from
 * namespace URI onto schema.
 *
 * Constructing these from a list of schema Nodes is a unit-test
 * interface.
 *
 * schemaNodeList is a list of scala xml Node, each expected to be an <xs:schema...>
 * node. These may have include/import statements in them, and the schemas
 * being included/imported don't have to be within the list.
 *
 */
class SchemaSet(schemaNodeList: Seq[Node]) {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.

  lazy val schemaPairs = schemaNodeList.map { s =>
    {
      val ns = (s \ "@targetNamespace").text
      (ns, s)
    }
  }

  lazy val schemaGroups = schemaPairs.groupBy {
    case (ns, s) => ns
  }.toList

  lazy val schemas = schemaGroups.map {
    case (ns, pairs) => {
      val sds = pairs.map(_._2) // Grabs second of 'pairs', list of schema Document
      val res = new Schema(ns, sds, this)
      res
    }
  }

  /**
   * Retrieve schema by namespace name.
   */
  def getSchema(namespace: String) = {
    // TODO: what about when there is no namespace. Can we pass "" ??
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }
  
  /**
   * Given a namespace and name, try to retrieve the named object
   */
  def getGlobalElementDecl(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
  def getGlobalSimpleTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }
  def getDefineFormat(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineFormat(name) }
  def getDefineVariable(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineVariable(name) }
  def getDefineEscapeScheme(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

}

/**
 * A schema is all the schema documents sharing a single target namespace.
 *
 * That is, one can write several schema documents which all have the
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema(val namespace: String, val schemaDocs: NodeSeq, val schemaSet: SchemaSet) {

  lazy val targetNamespace = namespace

  lazy val schemaDocuments = schemaDocs.map { new SchemaDocument(_, this) }

  
  private def noneOrOne[T](scs: Seq[T], name: String) : Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc)
      case _ => throw new DFDLSchemaDefinitionException("more than one definition for name: " + name)
    }
  }

  /**
   * Given a name, try to retrieve the appropriate object.
   */
  def getGlobalElementDecl(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalElementDecl(name) }, name)
  def getGlobalSimpleTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalSimpleTypeDef(name) }, name)
  def getGlobalComplexTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalComplexTypeDef(name) }, name)
  def getGlobalGroupDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalGroupDef(name) }, name)
  def getDefineFormat(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineFormat(name) }, name)
  def getDefineVariable(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineVariable(name) }, name)
  def getDefineEscapeScheme(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineEscapeScheme(name) }, name)

}

/**
 * A schema document corresponds to one file usually named with an ".xsd" extension.
 * The root element of a schema document is xsd:schema where xsd is the namespace for
 * XML Schema.
 *
 * A schema document is important because it is the unit of lexical scoping of format
 * properties in DFDL.
 *
 * Specifically, note that two schema documents may have the same target namespace, but
 * different default formats scoped over their contents.
 *
 * When dealing with plain XML and XSD (not DFDL), the concept of Schema with a single
 * target namespace is more used than the concept of schema document, which is usually
 * only needed to issue diagnostic error messages ("In file foo.xsd, line 938...")
 *
 * Conversely, for DFDL, the concept of schema document is more used because schema documents
 * are where default formats are specified, so it is very important what schema document
 * a schema component was defined within.
 */
class SchemaDocument(xmlArg: Node, schemaArg: => Schema) extends AnnotatedMixin {
  //
  // schemaArg is call by name, so that in the corner case of NoSchemaDocument (used for non-lexically enclosed annotation objects), 
  // we can pass an Assert.invariantFailed to bomb if anyone actually tries to use the schema.
  //
  // This is one of the techniques we use to avoid using null and having to test for null.
  //

  override lazy val schema = schemaArg
  lazy val xml = xmlArg
  lazy val targetNamespace = schema.targetNamespace
  lazy val schemaDocument = this

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:format>{ content @ _* }</dfdl:format> => new DFDLFormat(node, this)
      case <dfdl:defineFormat>{ content @ _* }</dfdl:defineFormat> => new DFDLDefineFormat(node, this)
      case <dfdl:defineEscapeScheme>{ content @ _* }</dfdl:defineEscapeScheme> => new DFDLDefineEscapeScheme(node, this)
      case <dfdl:defineVariable>{ content @ _* }</dfdl:defineVariable> => new DFDLDefineVariable(node, this)
      case _ => Assert.impossible("Invalid dfdl annotation found!")
    }
  }

  def emptyFormatFactory = new DFDLFormat(<dfdl:format/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

  private lazy val sset = schema.schemaSet

  lazy val globalElementDecls = (xml \ "element").map { new GlobalElementDecl(_, this) }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDef(_, this) }


  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]
  
  lazy val defaultProperties = defaultFormat.combinedLocalProperties

  //
  // There's some scala way of avoiding all this downcasting, using some clever type parameterization scheme
  // Have to ask a Scala types expert.
  //
  lazy val defineFormats = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineFormat] }.map{_.asInstanceOf[DFDLDefineFormat]}
    df
  }

  lazy val defineEscapeSchemes = {
    val desc = annotationObjs.filter { _.isInstanceOf[DFDLDefineEscapeScheme] }.map{_.asInstanceOf[DFDLDefineEscapeScheme]}
    desc
  }
  
  lazy val defineVariables = {
    val dv = annotationObjs.filter { _.isInstanceOf[DFDLDefineVariable] }.map{_.asInstanceOf[DFDLDefineVariable]}
    dv
  }

  /**
   * Issue: cloning, so that each referer gets a copy of the object
   * TBD: don't design so as to rule-out recursion some day.
   */
  def getGlobalElementDecl(name: String) = globalElementDecls.find { _.name == name }
  def getGlobalSimpleTypeDef(name: String) = globalSimpleTypeDefs.find { _.name == name }
  def getGlobalComplexTypeDef(name: String) = globalComplexTypeDefs.find { _.name == name }
  def getGlobalGroupDef(name: String) = globalGroupDefs.find { _.name == name }
  def getDefineFormat(name: String) = defineFormats.find { _.name == name }
  def getDefineVariable(name: String) = defineVariables.find { _.name == name }
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}


/**
 * Singleton to use when something usually has a schema document it refers to, but sometimes doesn't but you 
 * have to supply something. Use this.
 * 
 * This is an alternative to everybody having to use Option[SchemaDocument] for these corner cases, or passing null, etc.
 */
object NoSchemaDocument extends SchemaDocument(
    <schema/>, // dummy piece of XML that has no attributes, no annotations, no children, etc. Convenient to avoid conditional tests.
    Assert.invariantFailed("object NoSchemaDocument has no schema.")
    )

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
extends AnnotatedElementMixin {
   
   def isNillable = (xml \ "@nillable").text == "true"
   def isSimpleType : Boolean
   def isComplexType : Boolean
   def elementComplexType : ComplexTypeBase = typeDef.asInstanceOf[ComplexTypeBase]
   def elementSimpleType : SimpleTypeBase = typeDef.asInstanceOf[SimpleTypeBase]
   def typeDef : TypeBase
   
  /**
   * Grammar
   */
   
  object elementInitiator extends Production(this, NYI)
  object elementTerminator extends Production(this, NYI)
  object nilElementInitiator extends Production(this, NYI)
  object nilElementTerminator extends Production(this, NYI)
  object literalNilValue extends Production(this, NYI)
       
  object complexContent extends Production(this, isComplexType, elementInitiator ~ elementComplexType.grammarExpr ~ elementTerminator)
   
  object nilLit extends Production(this,
      isNillable && nilKind == NilKind.LiteralValue, 
      nilElementInitiator ~ literalNilValue ~ nilElementTerminator )
  
  object scalarSimpleContent extends Production(this, NYI) // nilLit | emptyDefaulted | parsedNil | parsedValue )

  object scalarComplexContent extends Production(this, nilLit | complexContent )
  
  object scalarContent extends Production(this, scalarSimpleContent | scalarComplexContent)
  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  object elementLeftFraming extends Production(this, NYI, EmptyExpr) // (leadingSkipParser ~ alignmentFill ~ prefixLength)

  object elementRightFraming extends Production(this, NYI, EmptyExpr) // trailingSkipParser

  object scalar extends Production(this, false, elementLeftFraming ~ scalarContent ~ elementRightFraming) 
}

abstract class LocalElementBase(xmlArg: Node, parent: ModelGroup)
  extends Term(xmlArg, parent)
  with ElementBaseMixin
  with Particle {

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def separatedForPosition(contentBody : Expr): Expr = {
    val Some(res) = nearestEnclosingSequence.map{_.separatedForPosition(contentBody)}
    res
  }
  
  def grammarExpr = term
  
  object arrayContents extends Production(this, NYI)
  object finalUnusedRegion extends Production(this, NYI) // probably this is really a primitive
  object separatedScalar extends Production(this, isScalar, separatedForPosition(scalar))
  object recurrance extends Production(this, !isScalar, startArray(this) ~ arrayContents ~ endArray(this) ~ finalUnusedRegion)
  
  // FIXME: doesn't allow for an element inside a choice, that is inside a sequence. Or a nest of nothing but choices. (No sequences at all)
  object term extends Production(this, separatedScalar | recurrance)
 
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
  with DFDLStatementMixin {
  lazy val xml = xmlArg
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactory(node, this)
    }
  }
  
  /**
   * DFDL Grammar
   */
  
  object documentElement extends Production(this,  scalar )
  
  object unicodeByteOrderMark extends UnicodeByteOrderMark(this, NYI)
  
  object document extends Production(this, unicodeByteOrderMark ~ documentElement )
}



/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xmlArg: Node, val parent: SchemaComponent)
  extends Annotated(xmlArg)
  with LocalComponentMixin
  with DFDLStatementMixin {
  def annotationFactory(node: Node): DFDLAnnotation = annotationFactory(node, this)
  
  def grammarExpr : Expr
  
  lazy val nearestEnclosingSequence : Option[Sequence] = {
    val res = parent match {
      case s : Sequence => Some(s)
      case c : Choice => c.nearestEnclosingSequence
      case d : SchemaDocument => None
      // We should only be asking for the enclosingSequence when there is one.
      case _ => Assert.invariantFailed("No enclosing sequence for : " + this)
    }
    res
  }
  
  import daffodil.util.ListUtils
  
  lazy val hasLaterRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.tailAfter _)
  lazy val hasPriorRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.preceding _)
  
  def hasRequiredSiblings(f : ListUtils.SubListFinder[Term]) = {
    val res = nearestEnclosingSequence.map{es=>{
        val sibs = f(es.groupMembers, this)
        val hasAtLeastOne = sibs.find { term => term.hasStaticallyRequiredInstances }
        hasAtLeastOne != None
      }
    }.getOrElse(false)
    res
  }
  
  def hasStaticallyRequiredInstances : Boolean
}

/**
 * Factory for Terms 
 *
 * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for
 * an actual term. There should be only one non-Nil.
 */
object Term {
  def apply(child: Node, parent : ModelGroup) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        if (refProp == "") List(new LocalElementDecl(child, parent))
        else List(new ElementRef(child, parent))
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => ModelGroup(child, parent)
    }
    childList
  }
}

abstract class GroupBase(xmlArg: Node, parent: SchemaComponent)
  extends Term(xmlArg, parent) {
  
  def group : ModelGroup
}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parent: SchemaComponent) extends GroupBase(xmlArg, parent) {
    
   val xmlChildren : Seq[Node]
     
   private lazy val children = xmlChildren.flatMap { Term(_, this) }
   
   def group = this
   
   lazy val groupMembers = children 
  
   lazy val groupMemberGrammarNodes = groupMembers.map{ _.grammarExpr }
   
   /**
    * Grammar
    */
     
  object groupLeftFraming extends Production(this, NYI) // leadingSkipParser ~ alignmentFill ~ groupInitiator)
  object groupRightFraming extends Production(this, NYI) // groupTerminator ~ trailingSkipParser)
  
  object grammarExpr extends Production(this, groupLeftFraming ~ groupContent ~ groupRightFraming )
  
  def mt = EmptyExpr.asInstanceOf[Expr]// cast trick to shut up foldLeft compile error on next line.
  
  object groupContent extends Production(this, groupMemberGrammarNodes.foldLeft(mt)(folder) )
   
  def folder(p : Expr, q : Expr) : Expr
}

/**
 * A factory for model groups.
 */
object ModelGroup{
  
  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
    def apply(child : Node, self : SchemaComponent) = {
      val childList : List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, self))
      case <choice>{ _* }</choice> => List(new Choice(child, self))
      case <group>{ _* }</group> => List(new GroupRef(child, self))
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => Assert.impossibleCase()
    }
    childList
    }
    
}

class Choice(xmlArg: Node, parent: SchemaComponent) extends ModelGroup(xmlArg, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLChoice(<dfdl:choice/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]
  
  lazy val <choice>{ xmlChildren @ _* }</choice> = xml
  
  def folder(p : Expr, q : Expr) = p | q 
  
  lazy val hasStaticallyRequiredInstances = {
    // true if all arms of the choice have statically required instances.
    groupMembers.forall{_.hasStaticallyRequiredInstances}
  }
}

class Sequence(xmlArg: Node, parent: SchemaComponent) 
extends ModelGroup(xmlArg, parent)
with Sequence_AnnotationMixin
with SequenceRuntimeValuedPropertiesMixin 
{
  def getPropertyOption(pname : String) = formatAnnotation.getPropertyOption(pname)
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLSequence(<dfdl:sequence/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  lazy val <sequence>{ xmlChildren @ _* }</sequence> = xml

  def folder(p : Expr, q : Expr) = p ~ q 
  
  lazy val hasStaticallyRequiredInstances = {
    // true if any child of the sequence has statically required instances.
    groupMembers.exists{_.hasStaticallyRequiredInstances}
  }
  
  /**
   * Grammar
   */
  
  def separatedForPosition(contentBody : Expr): Expr = {
    prefixSep ~ infixSepRule ~ contentBody ~ postfixSep
  }
  
  object prefixSep extends Delimiter(this, hasPrefixSep)
  object postfixSep extends Delimiter(this, hasPostfixSep)
  object infixSep extends Delimiter(this, hasInfixSep)
  
  object infixSepWithPriorRequiredSiblings extends Production(this, hasInfixSep && hasPriorRequiredSiblings,
      infixSep)
  object infixSepWithoutPriorRequiredSiblings extends Production(this, hasInfixSep && !hasPriorRequiredSiblings,
      // runtime check for group pos such that we need a separator.
     groupPosGreaterThan(1)(this) ~ infixSep )
  
  object infixSepRule extends Production(this, hasInfixSep,
      infixSepWithPriorRequiredSiblings | infixSepWithoutPriorRequiredSiblings)
  
     /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)
  lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)
  lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  def sepExpr(pos: SeparatorPosition): Boolean = {
    if (separatorExpr.isKnownNonEmpty) if (separatorPosition eq pos) true else false
    else false
  }
  
}

class GroupRef(xmlArg : Node, parent: SchemaComponent) 
  extends GroupBase(xmlArg, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLGroup(<dfdl:group/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLGroup]
  
  def group = Assert.notYetImplemented()
  def grammarExpr = Assert.notYetImplemented()
  def hasStaticallyRequiredInstances = Assert.notYetImplemented()
}

class GlobalGroupDef(val xmlArg : Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin {
  lazy val xml = xmlArg
  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to map, so that we can tolerate annotation objects.
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  lazy val Seq(modelGroup) = xmlChildren.flatMap{ ModelGroup(_, this) }
  
}

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

abstract class NamedSimpleTypeBase(xmlArg : => Node, parent: => SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent) with NamedType {
}

class LocalSimpleTypeDef(xmlArg : Node, parent: SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent)
  with LocalComponentMixin {

  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
  
  lazy val base = (xml \ "restriction" \ "@base").text
  
}



//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) extends NamedSimpleTypeBase(
    Assert.invariantFailed("primitives don't have XML."), 
    Assert.invariantFailed("primitives don't have parents.")
    ) {
  //
  // Lots of faking & dummy objects here
  //
  override lazy val xml = <fake_primitive name={ name_ }/> // unused. we have to provide the definition in order to compile.
  lazy val xsdNamespace = XMLUtil.XSD_NAMESPACE
  lazy val dummySchemaSet = new SchemaSet(NodeSeq.Empty)
  lazy val xsdSchema = new Schema(xsdNamespace, NodeSeq.Empty, dummySchemaSet)
  lazy val schemaDocument = new SchemaDocument(<schema/>, xsdSchema)
  def emptyFormatFactory = Assert.invariantFailed()
  def isMyAnnotation(a : DFDLAnnotation) = Assert.invariantFailed()
}

class GlobalSimpleTypeDef(xmlArg : Node, val schemaDocument: SchemaDocument)
  extends NamedSimpleTypeBase(xmlArg, schemaDocument) with GlobalComponentMixin {
  
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
  
}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent) extends SchemaComponent with TypeBase {
  lazy val xml = xmlArg
  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap{ ModelGroup(_, this) }
  
  object grammarExpr extends Production(this, startGroup(this) ~ modelGroup.group.grammarExpr ~ endGroup(this))
}

class GlobalComplexTypeDef(xmlArg : Node, val schemaDocument: SchemaDocument)
  extends ComplexTypeBase(xmlArg, schemaDocument)
  with GlobalComponentMixin {
}

class LocalComplexTypeDef(xmlArg : Node, parent: SchemaComponent)
  extends ComplexTypeBase(xmlArg, parent)
  with LocalComponentMixin {
}
