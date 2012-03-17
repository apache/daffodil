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
abstract class Term(xmlArg: Node, val parent: SchemaComponent)
  extends Annotated(xmlArg)
  with LocalComponentMixin
  with DFDLStatementMixin
  with TermGrammarMixin {
  def annotationFactory(node: Node): DFDLAnnotation = annotationFactory(node, this)
  
  // TODO: verify this is not just lexical scope containing. It's the scope of physical containment, so 
  // must also take into consideration references (element ref to element decl, element decl to type, type to group,
  // group to group)
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
abstract class ModelGroup(xmlArg: Node, parent: SchemaComponent) 
extends GroupBase(xmlArg, parent) 
with ModelGroupGrammarMixin {
    
   val xmlChildren : Seq[Node]
     
   private lazy val children = xmlChildren.flatMap { Term(_, this) }
   
   def group = this
   
   lazy val groupMembers = children 
  
   lazy val groupMemberGrammarNodes = groupMembers.map{ _.grammarExpr }
  
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

class Choice(xmlArg: Node, parent: SchemaComponent) 
extends ModelGroup(xmlArg, parent)
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
}

class Sequence(xmlArg: Node, parent: SchemaComponent) 
extends ModelGroup(xmlArg, parent)
with Sequence_AnnotationMixin
with SequenceRuntimeValuedPropertiesMixin 
with SequenceGrammarMixin
with SeparatorSuppressionPolicyMixin
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
  
  lazy val hasStaticallyRequiredInstances = {
    // true if any child of the sequence has statically required instances.
    groupMembers.exists{_.hasStaticallyRequiredInstances}
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

