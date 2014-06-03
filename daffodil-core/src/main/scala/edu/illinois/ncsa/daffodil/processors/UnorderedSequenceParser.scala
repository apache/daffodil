package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.dsom.Sequence
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.debugger.Debugger
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.dsom.LocalElementDecl
import edu.illinois.ncsa.daffodil.dsom.LocalElementBase
import edu.illinois.ncsa.daffodil.grammar.UnaryGram
import scala.xml.Elem
import edu.illinois.ncsa.daffodil.xml.NS

object UnorderedSequence {
  def apply(context: Term, eGram: Gram) = {
    // mandatory little optimization here. If there are no statements (most common case), then let's 
    // shortcut and just use the guts parser.

    Assert.usageErrorUnless(context.isInstanceOf[Sequence], "The context passed to UnorderedSequence must be a Sequence.")

    val ctxt = context.asInstanceOf[Sequence]

    new UnorderedSequence(ctxt, eGram)
  }
}

class UnorderedSequence private (context: Sequence, eGram: Gram) // private to force use of the object as factory
  extends UnaryGram(context, eGram) {

  // Forced as part of required evaluations in Sequence
  //context.checkIfValidUnorderedSequence

  val uoSeqParser = eGram.parser
  val sortOrder = {
    val members = context.groupMembers.map(t => {
      t match {
        case s: Sequence => s.groupMembers
        case _ => Seq(t)
      }
    }).flatten

    members.map(t => {
      val (name, _) = t.nameAndPath
      val ns = t.xml.namespace
      (name, ns)
    })
  }

  val scalarMembers =
    context.groupMembers.filter(t => t.isInstanceOf[ElementBase]).filter {
      case eb: ElementBase => eb.isScalar
    }.map {
      case eb: ElementBase => (eb.name, eb.nameAndPath._2, eb.targetNamespace.toJDOM)
    }

  def parser: Parser = new UnorderedSequenceParser(context)

  class UnorderedSequenceParser(context: Sequence) extends PrimParser(this, context) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<UnorderedSequence>" + uoSeqParser +
          "</UnorderedSequence>"
    }

    def sort(elt: org.jdom2.Element, pstate: PState): Unit = {
      val childrenDetached = elt.removeContent().toList.asInstanceOf[List[org.jdom2.Element]]

      sortOrder.foreach {
        case (name, ns) => {
          val newContent = childrenDetached.filter(child => {
            val infoSetElem = new InfosetElement(child)
            val sc = infoSetElem.schemaComponent(pstate)
            val isMatch = sc.prettyName == name && sc.xml.namespace == ns
            isMatch
          })

          if (newContent.size > 0)
            elt.addContent(newContent)
        }
      }
    }

    def checkScalarsOccurExactlyOnce(elt: org.jdom2.Element, pstate: PState): Unit = {
      // Always occurs, does not depend on validation
      val children = elt.getChildren().toList.asInstanceOf[List[org.jdom2.Element]]
      scalarMembers.foreach {
        case (name, path, ns) => {
          val scalarChildren = children.filter(e => {
            e.getName() == name && e.getNamespace() == ns
          })
          val numScalarChildren = scalarChildren.length
          if (numScalarChildren > 1 || numScalarChildren == 0)
            pstate.SDE("%s is scalar but occurred %s times.\nPath: %s\nOffending children: %s",
              name, scalarChildren.length, path, scalarChildren.mkString(", "))
        }
      }
    }

    /**
     * We want to check that we met he expected occurrence values
     * but we don't want to SDE, instead we issue a PE (ProcessingError).
     */
    def checkOccurrence(elt: org.jdom2.Element): Unit = {
      val childrenList = elt.getContent().toList.asInstanceOf[List[org.jdom2.Element]]

      val scs =
        context.groupMembers.filter(t => t.isInstanceOf[LocalElementBase]).map(_.asInstanceOf[LocalElementBase])

      scs.foreach(eb => {
        val minOccurs = eb.minOccurs
        val maxOccurs = eb.maxOccurs
        val ns = eb.targetNamespace.toJDOM
        val name = eb.name

        val children = childrenList.filter(c => {
          val childName = c.getName()
          val childNS = c.getNamespace()
          name == childName && ns == childNS
        })

        val numChildren = children.length

        if (numChildren < minOccurs) {
          PE("UnorderedSequence.checkOccurrence - %s failed minOccurs check. Expected at least %s but found %s.",
            eb, minOccurs, numChildren)
        }
        if (numChildren > maxOccurs) {
          PE("UnorderedSequence.checkOccurrence - %s failed maxOccurs check. Expected at most %s but found %s.",
            eb, maxOccurs, numChildren)
        }
      })
    }

    def dropDefaulted(elt: org.jdom2.Element, pstate: PState): Unit = {
      // Always occurs, does not depend on validation

      // RequiredElement, index in its array <= minOccurs
      // If empty rep, a required element that has a default value will be defaulted.

      // Drop anything after the minOccurs that was defaulted.

      // So we need the original minOccurs for each element.
      val childrenDetached = elt.removeContent().toList.asInstanceOf[List[org.jdom2.Element]]
      val scs = childrenDetached.map(c => {
        val infoSetElem = new InfosetElement(c)
        val sc = infoSetElem.schemaComponent(pstate)
        sc.asInstanceOf[LocalElementBase]
      }).distinct

      scs.foreach(eb => {
        val minOccurs = eb.minOccurs
        val ns = eb.targetNamespace.toJDOM
        val name = eb.name

        val children = childrenDetached.filter(c => {
          val childName = c.getName()
          val childNS = c.getNamespace()
          name == childName && ns == childNS
        })

        val numChildren = children.length

        if (numChildren >= minOccurs) {
          val firstChild = children.head
          val restOfChildren = children.tail
          val restOfNonDefaultedChildren = restOfChildren.filterNot(c => c.getAttribute("defaulted", XMLUtils.INT_NS_OBJECT).getBooleanValue())
          val newContent = firstChild :: restOfNonDefaultedChildren
          if (newContent.length > 0)
            elt.addContent(newContent)
        } else {
          // Nothing to do here, reattach children.
          elt.addContent(childrenDetached)
        }
      })
    }

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      val end = uoSeqParser.parse1(start, context)
      val currentElemAfter = end.parentElement.jdomElt.get

      checkScalarsOccurExactlyOnce(currentElemAfter, end)
      dropDefaulted(currentElemAfter, end)
      checkOccurrence(currentElemAfter)

      // Sort so that the contents are in the expected order.
      sort(currentElemAfter, end)

      end
    }
  }

  def unparser: Unparser = new Unparser(context) {
    def unparse(start: UState): UState = {
      val eUnParser = eGram.unparser
      val postEState = eUnParser.unparse(start)
      postEState
    }
  }

}
