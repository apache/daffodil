package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml._
import daffodil.exceptions._
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

