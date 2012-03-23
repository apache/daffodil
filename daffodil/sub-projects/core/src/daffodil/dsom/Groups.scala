package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml._
import daffodil.exceptions._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import daffodil.grammar._

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xmlArg : Node, val parent : SchemaComponent, val position : Int)
  extends Annotated(xmlArg)
  with LocalComponentMixin
  with DFDLStatementMixin
  with TermGrammarMixin {

  def annotationFactory(node : Node) : DFDLAnnotation = annotationFactory(node, this)

  def isScalar = true // override in local elements
  
/**
 * nearestEnclosingSequence 
 * 
 * An attribute that looks upward to the surrounding 
 * context of the schema, and not just lexically surrounding context. It needs to see 
 * what declarations will physically surround the place. This is the dynamic scope, 
 * not just the lexical scope. So, a named global type still has to be able to 
 * ask what sequence is surrounding the element that references the global type.
 * 
 * This is why we have to have the GlobalXYZDefFactory stuff. Because this kind of back
 * pointer (contextual sensitivity) prevents sharing.
 */
  lazy val nearestEnclosingSequence : Option[Sequence] = {
  // TODO: verify this is not just lexical scope containing. It's the scope of physical containment, so 
  // must also take into consideration references (element ref to element decl, element decl to type, type to group,
  // groupref to group)
    val res = parent match {
      case s : Sequence => Some(s)
      case c : Choice => c.nearestEnclosingSequence
      case d : SchemaDocument => None
      case ct : LocalComplexTypeDef => ct.parent match {
        case local : LocalElementDecl => local.nearestEnclosingSequence
        case global : GlobalElementDecl => {
          global.elementRef match {
              case None => None
              case Some(eRef) => eRef.nearestEnclosingSequence
            }
        }
        case _ => Assert.impossibleCase()
      }
      // global type, we have to follow back to the element referencing this type
      case ct : GlobalComplexTypeDef => {
        // Since we are a term directly inside a global complex type def,
        // our nearest enclosing sequence is the one enclosing the element that
        // has this type. 
        //
        // However, that element might be local, or might be global and be referenced
        // from an element ref.
        //
        ct.element match {
          case local : LocalElementDecl => local.nearestEnclosingSequence
          case global : GlobalElementDecl => {
            global.elementRef match {
              case None => None
              case Some(eRef) => eRef.nearestEnclosingSequence
            }
          }
          case _ => Assert.impossibleCase()
        }
      }
      case gd : GlobalGroupDef => gd.groupRef.nearestEnclosingSequence
      // We should only be asking for the enclosingSequence when there is one.
      case _ => Assert.invariantFailed("No enclosing sequence for : " + this)
    }
    res
  }

  lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import daffodil.util.ListUtils

  lazy val hasLaterRequiredSiblings : Boolean = hasRequiredSiblings(ListUtils.tailAfter _)
  lazy val hasPriorRequiredSiblings : Boolean = hasRequiredSiblings(ListUtils.preceding _)

  def hasRequiredSiblings(splitter : ListUtils.SubListFinder[Term]) = {
    val res = nearestEnclosingSequence.map { es =>
      {
        val allSiblings = es.groupMembers
        val sibs = splitter(allSiblings, this)
        val hasAtLeastOne = sibs.find { term => term.hasStaticallyRequiredInstances }
        hasAtLeastOne != None
      }
    }.getOrElse(false)
    res
  }

  def hasStaticallyRequiredInstances : Boolean

}

abstract class GroupBase(xmlArg: Node, parent: SchemaComponent, position : Int)
  extends Term(xmlArg, parent, position) {
  
  lazy val detailName = ""
  def group : ModelGroup
}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parent: SchemaComponent, position : Int) 
extends GroupBase(xmlArg, parent, position) 
with ModelGroupGrammarMixin {
    
   val xmlChildren : Seq[Node]
     
   private val goodXmlChildren = xmlChildren.flatMap{removeNonInteresting(_)}
   private val positions = List.range(1, goodXmlChildren.length + 1 ) // range is exclusive on 2nd arg. So +1.
   private val pairs = goodXmlChildren zip positions
   private lazy val children = pairs.flatMap{ 
     case (n, i) => 
       termFactory(n, this, i) 
       }
   
   def group = this
   
   lazy val groupMembers = {
     children
   }

  /**
   * Factory for Terms
   *
   * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for
   * an actual term. There should be only one non-Nil.
   * 
   * This could be static code in an object. It doesn't reference any of the state of the ModelGroup,
   * it's here so that type-specific overrides are possible in Sequence or Choice
   */
  def termFactory(child : Node, parent : ModelGroup, position : Int) = {
    val childList : List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        if (refProp == "") List(new LocalElementDecl(child, parent, position))
        else List(new ElementRef(child, parent, position))
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode : Text => Nil
      case _ => GroupFactory(child, parent, position)
    }
    childList
  } 
   
  def removeNonInteresting(child : Node) = {
    val childList : List[Node] = child match {
      case _ : Text => Nil
      case <annotation>{ _* }</annotation> => Nil
      case _ => List(child)
    }
    childList
  }
  
}

/**
 * A factory for model groups.
 */
object GroupFactory{
  
  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
    def apply(child : Node, parent : SchemaComponent, position : Int) = {
      val childList : List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, parent, position))
      case <choice>{ _* }</choice> => List(new Choice(child, parent, position))
      case <group>{ _* }</group> => {
        parent match {
          case ct : ComplexTypeBase => List(new GroupRef(child, ct, 1))
          case mg : ModelGroup => List(new GroupRef(child, mg, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => Assert.impossibleCase()
    }
    childList
    }
    
}
/**
 * Choices are a bit complicated.
 * 
 * They can have initiators and terminators. This is most easily thought of as wrapping each choice 
 * inside a sequence having only the choice inside it, and moving the initiator and terminator specification to that 
 * sequence.
 * 
 * That sequence is then the term replacing the choice wherever the choice is.
 * 
 * Choices can have children which are scalar elements. In this case, that scalar element behaves as if it were 
 * a child of the enclosing sequence (which could be the one we just injected above the choice.
 * 
 * Choices can have children which are recurring elements. In this case, the behavior is as if the recurring child was
 * placed inside a sequence which has no initiator nor terminator, but repeats the separator specification from 
 * the sequence context that encloses the choice. 
 * 
 * All that, and the complexities of separator suppression too. 
 * 
 * There's also issues like this:
 * 
 * <choice>
 *    <element .../>
 *    <sequence/>
 * </choice>
 * 
 * in the above, one alternative is an empty sequence. So this choice may produce an element which takes up 
 * a child position, and potentially requires separation, or it may produce nothing at all. 
 * 
 * So, to keep things managable, we're going to start with some restrictions
 * 
 * 1) all children of a choice must be scalar elements
 * 2) no initiators nor terminators on choices. (Just wrap in a sequence if you care.)
 * 
 */

class Choice(xmlArg: Node, parent: SchemaComponent, position : Int) 
extends ModelGroup(xmlArg, parent, position)
with ChoiceGrammarMixin {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLChoice(<dfdl:choice/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]
  
  lazy val <choice>{ xmlChildren @ _* }</choice> = xml
  
  lazy val hasStaticallyRequiredInstances = {
    // true if all arms of the choice have statically required instances.
    groupMembers.forall{_.hasStaticallyRequiredInstances}
  }
  
  /**
   * We override termFactory because we're only going to support a subset of the full
   * generality of what could go inside a choice.
   * 
   * TODO: someday lift this restriction.
   */
  override def termFactory(child: Node, parent : ModelGroup, position : Int) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        val elt = 
          if (refProp == "") new LocalElementDecl(child, parent, position)
          else new ElementRef(child, parent, position)
        Assert.subset(elt.isScalar, "Choices may only have scalar element children (minOccurs = maxOccurs = 1).")
        List(elt)
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => Assert.subset("Non-element child type. Choices may only have scalar element children (minOccurs = maxOccurs = 1).")
    }
    childList
  } 
}

class Sequence(xmlArg: Node, parent: SchemaComponent, position : Int) 
extends ModelGroup(xmlArg, parent, position)
with Sequence_AnnotationMixin
with SequenceRuntimeValuedPropertiesMixin 
with SequenceGrammarMixin
with SeparatorSuppressionPolicyMixin
{
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLSequence(<dfdl:sequence/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  lazy val <sequence>{ xmlChildren @ _* }</sequence> = xml
  
  lazy val hasStaticallyRequiredInstances = {
    // true if any child of the sequence has statically required instances.
    groupMembers.exists{_.hasStaticallyRequiredInstances}
  }
    
}

class GroupRef(xmlArg : Node, parent: SchemaComponent, position : Int) 
  extends GroupBase(xmlArg, parent, position) 
  with GroupRefGrammarMixin {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLGroup(<dfdl:group/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLGroup]
  
  def group = Assert.notYetImplemented()

  def hasStaticallyRequiredInstances = Assert.notYetImplemented()
  
   lazy val refName = {
    val str = (xml \ "@ref").text
    if (str == "") None else Some(str)
  }



  lazy val refQName = {
    refName match {
      case Some(rname) => Some(XMLUtil.QName(xml, rname, schemaDocument))
      case None => None
    }
  }

  lazy val groupDef: Option[GlobalGroupDef] = {
    refQName match {
      case None => None
      case Some((ns, localpart)) => {

        val ss = schema.schemaSet
        val ggdf = ss.getGlobalGroupDef(ns, localpart)
        val res = ggdf match {
            case Some(ggdFactory) => Some(ggdFactory.forGroupRef(this, position))
            case None => Assert.schemaDefinitionError("No group definition found for " + refName + ".")
            // FIXME: do we need to do these checks, or has schema validation checked this for us?
            // FIXME: if we do have to check, then the usual problems: don't stop on first error, and need location of error in diagnostic.
          }
          res
        }
      }
    }

}

class GlobalGroupDefFactory(val xmlArg : Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin {
  def xml = xmlArg
 
//  def forComplexType(ct : ComplexTypeBase) = {
//    new GlobalGroupDef(xmlArg, schemaDocument, ct, 1)
//  }
  
  def forGroupRef(gref : GroupRef, position : Int) = {
    new GlobalGroupDef(xmlArg, schemaDocument, gref, position)
  }
}

class GlobalGroupDef(val xmlArg : Node, val schemaDocument: SchemaDocument, val groupRef : GroupRef, position : Int)
  extends GlobalComponentMixin {
  lazy val xml = xmlArg
  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to flatMap, so that we can tolerate annotation objects (like documentation objects).
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  lazy val Seq(modelGroup :  ModelGroup) = xmlChildren.flatMap{ GroupFactory(_, this, position) }
  
}

