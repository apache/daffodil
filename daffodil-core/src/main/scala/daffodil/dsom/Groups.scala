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
abstract class Term(xmlArg: Node, val parent: SchemaComponent, val position: Int)
  extends AnnotatedSchemaComponent(xmlArg)
  with LocalComponentMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin {

  lazy val someEnclosingComponent = enclosingComponent.getOrElse(Assert.invariantFailed("All terms except a root element have an enclosing component."))

  lazy val enclosingComponent: Option[SchemaComponent] = {
    val res = Some(parent) // for global objects, the enclosing will be the thing referencing them.
    res
  }

  lazy val isRepresented = true // overridden by elements, which might have inputValueCalc turning this off

  def isScalar = true // override in local elements

  lazy val allTerminatingMarkup: List[CompiledExpression] = {
    val tm = List(this.terminator) ++ this.allParentTerminatingMarkup
    tm.filter(x => x.isKnownNonEmpty)
  }

  // TODO Review Comment
  // This below should not reproduce the logic of enclosingComponent unless it needs
  // something different from that. 

  lazy val allParentTerminatingMarkup: List[CompiledExpression] = {
    // Retrieves the terminating markup for all parent
    // objects
    // println(this + " with parent " + parent)

    //
    val pTM = parent match {
      case s: Sequence => List(s.separator, s.terminator) ++ s.allParentTerminatingMarkup
      case c: Choice => c.allParentTerminatingMarkup
      case d: SchemaDocument =>
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.allTerminatingMarkup
          case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing sequence at all
              List.empty
            }
            case Some(er) => er.allTerminatingMarkup
          }
        }
      case ct: LocalComplexTypeDef => ct.parent match {
        case local: LocalElementDecl => local.allTerminatingMarkup
        case global: GlobalElementDecl => {
          global.elementRef match {
            case None => List(global.terminator)
            case Some(eRef) => eRef.allTerminatingMarkup
          }
        }
        case _ => Assert.impossibleCase()
      }
      // global type, we have to follow back to the element referencing this type
      case ct: GlobalComplexTypeDef => {
        // Since we are a term directly inside a global complex type def,
        // our nearest enclosing sequence is the one enclosing the element that
        // has this type. 
        //
        // However, that element might be local, or might be global and be referenced
        // from an element ref.
        //
        ct.element match {
          case local: LocalElementDecl => local.allTerminatingMarkup
          case global: GlobalElementDecl => {
            global.elementRef match {
              case None => List(global.terminator)
              case Some(eRef) => eRef.allTerminatingMarkup
            }
          }
          case _ => Assert.impossibleCase()
        }
      }
      case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
      // We should only be asking for the enclosingSequence when there is one.
      case _ => Assert.invariantFailed("No parent terminating markup for : " + this)
    }
    val res = pTM.filter(x => x.isKnownNonEmpty)
    // println(res)
    res
  }

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
  // TODO Review Comment
  // This below should not reproduce the logic of enclosingComponent unless it needs
  // something different from that. 
  lazy val nearestEnclosingSequence: Option[Sequence] = nearestEnclosingSequence_.value
  private lazy val nearestEnclosingSequence_ = LV('nearestEnclosingSequence) {
    val res = parent match {
      case s: Sequence => Some(s)
      case t: Term => t.nearestEnclosingSequence
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.nearestEnclosingSequence
          case gd: GlobalGroupDef => gd.groupRef.nearestEnclosingSequence
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing sequence at all
              None
            }
            case Some(er) => er.nearestEnclosingSequence
          }
        }
      }
      case ct: ComplexTypeBase => None // Stop when we get to an element // ct.element.nearestEnclosingSequence
      case gd: GlobalGroupDef => gd.groupRef.nearestEnclosingSequence
      case _ => Assert.invariantFailed("nearestEnclosingSequence called on " + this + "with parent " + parent)
    }
    res
  }

  lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
    val res = parent match {
      case c: Choice => Some(c)
      case s: Sequence => Some(s)
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.immediatelyEnclosingModelGroup
          case gd: GlobalGroupDef => gd.groupRef.immediatelyEnclosingModelGroup
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing model group at all
              None
            }
            case Some(er) => er.immediatelyEnclosingModelGroup
          }
        }
      }
      case gdd: GlobalGroupDef => gdd.groupRef.immediatelyEnclosingModelGroup
      case ct: ComplexTypeBase => {
        None
        // The above formerly was ct.element.immediatelyEnclosingModelGroup, 
        // but if we have a CT as our parent, the group around the element whose type 
        // that is, isn't "immediately enclosing".
      }
      case _ => Assert.invariantFailed("immediatelyEnclosingModelGroup called on " + this + "with parent " + parent)
    }
    res
  }

  lazy val positionInNearestEnclosingSequence: Int = {
    val res =
      if (enclosingComponent == nearestEnclosingSequence) position
      else {
        enclosingComponent match {
          case Some(term: Term) => term.positionInNearestEnclosingSequence
          case Some(ct: ComplexTypeBase) => {
            val ctElem = ct.element
            val ctPos = ctElem.positionInNearestEnclosingSequence
            ctPos
          }
          case Some(ggd: GlobalGroupDef) => ggd.groupRef.positionInNearestEnclosingSequence
          case _ => Assert.invariantFailed("unable to compute position in nearest enclosing sequence")
        }
      }
    res
  }

  lazy val terminatingMarkup: List[CompiledExpression] = {
    if (hasTerminator) List(terminator)
    else nearestEnclosingSequence match {
      case None => Nil
      case Some(sq) => {
        val sep = {
          if (sq.hasInfixSep || sq.hasPostfixSep) List(sq.separator)
          else Nil
        }
        if (!hasLaterRequiredSiblings) {
          val entm = sq.terminatingMarkup
          val res = sep ++ entm
          res
        } else {
          sep
        }
      }
    }
  }

  lazy val prettyTerminatingMarkup =
    terminatingMarkup.map { _.prettyExpr }.map { "'" + _ + "'" }.mkString(" ")

  lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import daffodil.util.ListUtils

  lazy val hasLaterRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.tailAfter _)
  lazy val hasPriorRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.preceding _)

  def hasRequiredSiblings(splitter: ListUtils.SubListFinder[Term]) = {
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

  def hasStaticallyRequiredInstances: Boolean

}

abstract class GroupBase(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends Term(xmlArg, parent, position) {

  lazy val prettyIndex = {
    myPeers.map { peers =>
      {
        if (peers.length == 1) "" // no index expression if we are the only one
        else "[" + (peers.indexOf(this) + 1) + "]" // 1-based indexing in XML/XSD
      }
    }.getOrElse("")
  }

  lazy val prettyName = prettyBaseName + prettyIndex
  def prettyBaseName: String

  lazy val enclosingComponentModelGroup = enclosingComponent.collect { case mg: ModelGroup => mg }
  lazy val sequencePeers = enclosingComponentModelGroup.map { _.sequenceChildren }
  lazy val choicePeers = enclosingComponentModelGroup.map { _.choiceChildren }
  lazy val groupRefPeers = enclosingComponentModelGroup.map { _.groupRefChildren }

  def myPeers: Option[Seq[GroupBase]]

  def group: ModelGroup

  lazy val localAndFormatRefProperties = { this.formatAnnotation.getFormatPropertiesNonDefault() }

  lazy val localProperties = {
    // Properties that exist directly on the object in
    // short or long form
    this.formatAnnotation.combinedLocalProperties
  }

  lazy val formatRefProperties = {
    // Properties coming from named format ref from
    // the format annotation
    this.formatAnnotation.formatRefProperties
  }

  lazy val immediateGroup: Option[GroupBase] = {

    val res: Option[GroupBase] = this.group match {
      case (s: Sequence) => Some(s)
      case (c: Choice) => Some(c)
      case (g: GroupRef) => Some(g)
      case _ => None
    }

    res
  }

}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parent, position)
  with DFDLStatementMixin
  with ModelGroupGrammarMixin {

  lazy val prettyBaseName = xmlArg.label

  val xmlChildren: Seq[Node]

  private lazy val goodXmlChildren = goodXmlChildren_.value
  private lazy val goodXmlChildren_ = LV('goodXMLChildren) { xmlChildren.flatMap { removeNonInteresting(_) } }
  private lazy val positions = List.range(1, goodXmlChildren.length + 1) // range is exclusive on 2nd arg. So +1.
  private lazy val pairs = goodXmlChildren zip positions

  lazy val sequenceChildren = groupMembers.collect { case s: Sequence => s }
  lazy val choiceChildren = groupMembers.collect { case s: Choice => s }
  lazy val groupRefChildren = groupMembers.collect { case s: GroupRef => s }

  def group = this

  lazy val groupMembers = groupMembers_.value
  private lazy val groupMembers_ = LV('groupMembers) {
    pairs.flatMap {
      case (n, i) =>
        termFactory(n, this, i)
    }
  }

  lazy val diagnosticChildren = annotationObjs ++ groupMembers

  /**
   * Factory for Terms
   *
   * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for
   * an actual term. There should be only one non-Nil.
   *
   * This could be static code in an object. It doesn't reference any of the state of the ModelGroup,
   * it's here so that type-specific overrides are possible in Sequence or Choice
   */
  def termFactory(child: Node, parent: ModelGroup, position: Int) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        if (refProp == "") List(new LocalElementDecl(child, parent, position))
        else List(new ElementRef(child, parent, position))
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => GroupFactory(child, parent, position)
    }
    childList
  }

  /**
   * XML is full of uninteresting text nodes. We just want the element children, not all children.
   */
  def removeNonInteresting(child: Node) = {
    val childList: List[Node] = child match {
      case _: Text => Nil
      case <annotation>{ _* }</annotation> => Nil
      case _ => List(child)
    }
    childList
  }

  lazy val myGroupReferenceProps: Map[String, String] = {

    val noProps = Map.empty[String, String]
    parent match {
      case ggd: GlobalGroupDef => ggd.groupRef.localProperties //ggd.groupRef.localAndFormatRefProperties
      case mg: ModelGroup => noProps
      case ct: ComplexTypeBase => noProps
      case _ => Assert.invariantFailed("parent of group is not one of the allowed parent types.")
    }
  }

  lazy val overlappingProps: Set[String] = {
    val parentProps = myGroupReferenceProps.keySet
    val localProps = this.localAndFormatRefProperties.keySet
    val theIntersect = parentProps.intersect(localProps)
    theIntersect
  }

  lazy val combinedGroupRefAndGlobalGroupDefProperties: Map[String, String] = combinedGroupRefAndGlobalGroupDefProperties_.value
  private lazy val combinedGroupRefAndGlobalGroupDefProperties_ = LV('combinedGroupRefAndGlobalGroupDefProperties) {
    schemaDefinition(overlappingProps.size == 0,
      "Overlap detected between the properties in the model group of a global group definition (%s) and its group reference. The overlap: %s",
      this, overlappingProps)

    val props = myGroupReferenceProps ++ this.localAndFormatRefProperties
    props
  }

  override lazy val allNonDefaultProperties: Map[String, String] = allNonDefaultProperties_.value
  private lazy val allNonDefaultProperties_ = LV('allNonDefaultProperties) {
    val theLocalUnion = this.combinedGroupRefAndGlobalGroupDefProperties
    theLocalUnion
  }

  /**
   * Combine our statements with those of the group ref that is referencing us (if there is one)
   */
  lazy val statements: Seq[DFDLStatement] = localStatements ++ groupRef.map { _.statements }.getOrElse(Nil)
  lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    localNewVariableInstanceStatements ++ groupRef.map { _.newVariableInstanceStatements }.getOrElse(Nil)
  lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = localAssertStatements ++ groupRef.map { _.assertStatements }.getOrElse(Nil)
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = localDiscriminatorStatements ++ groupRef.map { _.discriminatorStatements }.getOrElse(Nil)

  lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = localSetVariableStatements ++ groupRef.map { _.setVariableStatements }.getOrElse(Nil)
    checkDistinctVariableNames(combinedSvs)
  }

  lazy val groupRef = parent match {
    case ggd: GlobalGroupDef => Some(ggd.groupRef)
    case _ => None
  }

}

/**
 * A factory for model groups.
 */
object GroupFactory {

  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
  def apply(child: Node, parent: SchemaComponent, position: Int) = {
    val childList: List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, parent, position))
      case <choice>{ _* }</choice> => List(new Choice(child, parent, position))
      case <group>{ _* }</group> => {
        parent match {
          case ct: ComplexTypeBase => List(new GroupRef(child, ct, 1))
          case mg: ModelGroup => List(new GroupRef(child, mg, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => {
        parent.SDE("Unrecognized construct: %s", child)
      }
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

class Choice(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with ChoiceGrammarMixin {

  lazy val myPeers = choicePeers

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLChoice(newDFDLAnnotationXML("choice"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]

  lazy val <choice>{ xmlChildren @ _* }</choice> = xml

  lazy val hasStaticallyRequiredInstances = {
    // true if the choice has syntactic features (initiator, terminator)
    hasInitiator || hasTerminator ||
      // or if all arms of the choice have statically required instances.
      groupMembers.forall { _.hasStaticallyRequiredInstances }
  }

  /**
   * We override termFactory because we're only going to support a subset of the full
   * generality of what could go inside a choice.
   *
   * TODO: someday lift this restriction.
   */
  override def termFactory(child: Node, parent: ModelGroup, position: Int) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        val elt =
          if (refProp == "") new LocalElementDecl(child, parent, position)
          else new ElementRef(child, parent, position)
        subset(elt.isScalar, "Choices may only have scalar element children (minOccurs = maxOccurs = 1).")
        List(elt)
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => subsetError("Non-element child type. Choices may only have scalar element children (minOccurs = maxOccurs = 1).")
    }
    childList
  }
}

class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SequenceGrammarMixin
  with SeparatorSuppressionPolicyMixin {

  lazy val myPeers = sequencePeers

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  lazy val hiddenGroupRefOption = getPropertyOption("hiddenGroupRef") //localProperties.get("hiddenGroupRef")

  /**
   * We're hidden if we're inside something hidden, or we're explicitly a
   * hidden group reference (sequence with hiddenGroupRef property)
   */
  override lazy val isHidden = {
    val res = hiddenGroupRefOption match {
      case Some(_) => true
      case None => someEnclosingComponent.isHidden
    }
    res
  }

  lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = xml

  lazy val xmlChildren = xmlChildren_.value
  private lazy val xmlChildren_ = LV('xmlChildren) {
    hiddenGroupRefOption match {
      case Some(qname) => {
        schemaDefinition(apparentXMLChildren.length == 0, "A sequence with hiddenGroupRef cannot have children.")
        // synthesize a group reference here.
        val hgr = <xs:group xmlns:xs={ XMLUtils.xsdURI } ref={ qname }/>
        List(hgr)
      }
      case None => apparentXMLChildren
    }
  }

  lazy val hasStaticallyRequiredInstances = {
    // true if there are syntactic features
    hasInitiator || hasTerminator ||
      // or if any child of the sequence has statically required instances.
      groupMembers.exists { _.hasStaticallyRequiredInstances }
  }

}

class GroupRef(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parent, position)
  with DFDLStatementMixin
  with GroupRefGrammarMixin
  with HasRef {

  lazy val prettyBaseName = "group.ref." + localName

  lazy val myPeers = groupRefPeers

  // BEGIN NEW CODE 10/30/2012

  lazy val qname = qname_.value
  private lazy val qname_ = LV('qname) { XMLUtils.QName(xml, xsdRef, schemaDocument) }

  lazy val (namespace, localName) = qname
  override lazy val localProperties = this.formatAnnotation.getFormatPropertiesNonDefault()
  override lazy val localAndFormatRefProperties = {
    // Removed check here for overlapping properties because it creates a circular reference
    // the check should instead take place in the referencedElement.
    val referencedProperties = group.localAndFormatRefProperties
    val myProperties = localProperties ++ referencedProperties
    myProperties
  }
  private lazy val referencedGroup_ = LV('referencedGroup) {
    this.schema.schemaSet.getGlobalGroupDef(namespace, localName) match {
      case None => SDE("Referenced groupDef not found: %s", this.ref)
      case Some(x) => x.forGroupRef(this, position)
    }
  }
  // END NEW CODE 10/30/2012

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLGroup(newDFDLAnnotationXML("group"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLGroup]

  def hasStaticallyRequiredInstances = group.hasStaticallyRequiredInstances

  //  // TODO: Consolidate techniques with HasRef trait used by ElementRef
  //  lazy val refName = {
  //    val str = (xml \ "@ref").text
  //    if (str == "") None else Some(str)
  //  }
  //
  //  lazy val refQName = {
  //    refName match {
  //      case Some(rname) => Some(XMLUtils.QName(xml, rname, schemaDocument))
  //      case None => None
  //    }
  //  }

  lazy val group = groupDef.modelGroup

  lazy val groupDef: GlobalGroupDef = referencedGroup_.value
  // 10/30/2012
  //lazy val groupDef: GlobalGroupDef = referencedGroup_.value//groupDef_.value

  //  private lazy val groupDef_ = LV {
  //    val res = refQName match {
  //      // TODO See comment above about consolidating techniques.
  //      case None => schemaDefinitionError("No group definition found for " + refName + ".")
  //      case Some((ns, localpart)) => {
  //        val ss = schema.schemaSet
  //        val ggdf = ss.getGlobalGroupDef(ns, localpart)
  //        val res = ggdf match {
  //          case Some(ggdFactory) => ggdFactory.forGroupRef(this, position)
  //          case None => schemaDefinitionError("No group definition found for " + refName + ".")
  //          // FIXME: do we need to do these checks, or has schema validation checked this for us?
  //          // FIXME: if we do have to check, then the usual problems: don't stop on first error, and need location of error in diagnostic.
  //        }
  //        res
  //      }
  //    }
  //    res
  //  }

  lazy val statements = localStatements
  lazy val newVariableInstanceStatements = localNewVariableInstanceStatements
  lazy val assertStatements = localAssertStatements
  lazy val discriminatorStatements = localDiscriminatorStatements
  lazy val setVariableStatements = localSetVariableStatements

  lazy val diagnosticChildren = annotationObjs :+ groupDef

}

class GlobalGroupDefFactory(val xml: Node, schemaDocument: SchemaDocument)
  extends NamedMixin {

  def forGroupRef(gref: GroupRef, position: Int) = {
    new GlobalGroupDef(xml, schemaDocument, gref, position)
  }
}

class GlobalGroupDef(val xmlArg: Node, val schemaDocument: SchemaDocument, val groupRef: GroupRef, position: Int)
  extends SchemaComponent(xmlArg) with GlobalComponentMixin {

  override lazy val prettyName = "group." + name

  lazy val enclosingComponent = {
    val res = Some(groupRef)
    res
  }
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
  lazy val Seq(modelGroup: ModelGroup) = xmlChildren.flatMap { GroupFactory(_, this, position) }

  lazy val diagnosticChildren = List(modelGroup)

}

