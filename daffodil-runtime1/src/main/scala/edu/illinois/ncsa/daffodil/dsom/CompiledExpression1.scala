/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.Suspension
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.util.TransientParam
import edu.illinois.ncsa.daffodil.util.Maybe
import scala.runtime.ScalaRunTime.stringOf // for printing arrays properly.
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.HasSchemaFileLocation
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.api.DaffodilTunables
import edu.illinois.ncsa.daffodil.api.UnqualifiedPathStepPolicy

trait ContentValueReferencedElementInfoMixin {

  def contentReferencedElementInfos: Set[DPathElementCompileInfo]
  def valueReferencedElementInfos: Set[DPathElementCompileInfo]
}
/**
 * For the DFDL path/expression language, this provides the place to
 * type check the expression (SDE if not properly typed)
 * and provides the opportunity to compile it for efficient evaluation.
 *
 * The schemaNode is the schema component
 * where the path is being evaluated which due to scoping, may not
 * be the same one where it is defined. It is the combination of a
 * property valued expression with a schema node that defines
 * an evaluation of an expression.
 *
 * TODO: Consider - that an expression could be constant in some contexts, not others.
 * E.g., if a DFDL schema defines a format where the delimiters are in a header record,
 * then those are constant once you are parsing the body records. This does imply
 * keeping around the xpath compiler at runtime, which may not be desirable from a
 * code size perspective. Whether it's worth it to compile or not is also a question
 * of how often each xpath will be repeated.
 *
 * TODO: provide enough scope information for this to optimize.
 */
abstract class CompiledExpression[+T <: AnyRef](
  val qName: NamedQName,
  valueForDebugPrinting: AnyRef)
  extends ContentValueReferencedElementInfoMixin with Serializable {

  final def toBriefXML(depth: Int = -1) = {
    "'" + prettyExpr + "'"
  }

  /**
   * Note use of the `stringOf(v)` below.
   * Turns out `x.toString` creates some crappy printed representations,
   * particularly for `Array[Byte]`. It prints a useless thing like "[@0909280".
   * Use of `stringOf` prints "Array(....)".
   */
  lazy val prettyExpr = stringOf(valueForDebugPrinting)

  /**
   * tells us if the property is non-empty. This is true if it is a constant non-empty expression
   * (that is, is not ""), but it is also true if it is evaluated as a runtime expression that it is
   * not allowed to return "".
   *
   * Issue: are there properties which are string-valued, and where "" can in fact be returned at run time?
   * Assumed no. This was clarified in an errata to the DFDL spec.
   */
  def isKnownNonEmpty: Boolean

  /**
   * used to obtain a constant value.
   *
   * isConstant must be true or this will throw.
   */
  @deprecated("2016-02-18", "Code should just call evaluate(...) on an Evaluatable object.")
  def constant: T
  def isConstant: Boolean

  def evaluate(state: ParseOrUnparseState): T

  /**
   * The target type of the expression. This is the type that we want the expression to create.
   */
  def targetType: NodeInfo.Kind

  /*
   * Note that since we can reference variables, and those might never have been read,
   * the act of evaluating them changes the variableMap state potentially.
   */

  /**
   * Use for outputValueCalc.
   *
   * The whereBlockedLocation is modified via its block(...) method to indicate where the
   * expression blocked (for forward progress checking).
   */
  def evaluateForwardReferencing(state: ParseOrUnparseState, whereBlockedLocation: Suspension): Maybe[T]

  override def toString(): String = "CompiledExpression(" + valueForDebugPrinting.toString + ")"

}

object ReferencedElementInfos {

  val None = Set.empty.asInstanceOf[Set[DPathElementCompileInfo]]

}

final case class ConstantExpression[+T <: AnyRef](
  qn: NamedQName,
  kind: NodeInfo.Kind,
  value: T) extends CompiledExpression[T](qn, value) {

  def targetType = kind

  lazy val sourceType: NodeInfo.Kind = NodeInfo.fromObject(value)

  def isKnownNonEmpty = value != ""

  override def evaluate(state: ParseOrUnparseState) = value

  def evaluate(dstate: DState, state: ParseOrUnparseState) = {
    dstate.setCurrentValue(value)
    value
  }

  final def evaluateForwardReferencing(state: ParseOrUnparseState, whereBlockedLocation: Suspension): Maybe[T] = {
    // whereBlockedLocation is ignored since a constant expression cannot block.
    whereBlockedLocation.setDone
    Maybe(evaluate(state))
  }

  def expressionEvaluationBlockLocation = MaybeULong.Nope

  def constant: T = value
  def isConstant = true

  override def contentReferencedElementInfos = ReferencedElementInfos.None
  override def valueReferencedElementInfos = ReferencedElementInfos.None
}

/**
 * This class is to contain only things that are needed to do
 * DPath Expression Compilation. Nothing else.
 *
 * This exists because some things have to be compiled (e.g., DPath expressions)
 * which then become part of the runtime data for elements or other.
 *
 * It becomes circular if all the information is bundled together on the
 * RuntimeData or ElementRuntimeData objects. So we split out
 * everything needed to compile expressions will get computed separately
 * (first), and kept on this object, and then subsequently ERD data
 * structures are created which reference these.
 *
 * In other words, it's just necessary layering of the different
 * phases of computation.
 *
 * Some of this dependency is artificial. If every individual attribute was
 * computed separately, none bundled together in common data structures,
 * AND everything was computed lazily, then this would probably all
 * just sort itself out and not be circular. What makes the circularity
 * is that the runtime data structures (ElementRuntimeData in particular),
 * are not lazy. Everything part of them is forced to be evaluated when those are
 * constructed. So anything that needs even one member of an ERD
 * is artificially dependent on *everything* in the ERD.
 *
 * Similarly these DPath compiler data structures.... anything that depends on them
 * is artificially dependent on ALL of their members's values.
 *
 * So the separation of DPath compiler info from runtime data structures is
 * really as close as we get in Daffodil to organizing the compilation of schemas
 * into "passes".
 */
class DPathCompileInfo(
  @TransientParam parentArg: => Option[DPathCompileInfo],
  @TransientParam variableMapArg: => VariableMap,
  val namespaces: scala.xml.NamespaceBinding,
  val path: String,
  override val schemaFileLocation: SchemaFileLocation,
  val tunable: DaffodilTunables)
  extends ImplementsThrowsSDE with PreSerialization
  with HasSchemaFileLocation {

  lazy val parent = parentArg
  lazy val variableMap =
    variableMapArg

  override def preSerialization: Any = {
    parent
    variableMap
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  override def toString = "DPathCompileInfo(%s)".format(path)
  /**
   * The immediate containing parent.
   */
  final lazy val immediateEnclosingCompileInfo: Option[DPathCompileInfo] = parent

  /**
   * The contract here supports the semantics of ".." in paths.
   *
   * First we establish the invariant of being on an element. If the
   * schema component is an element we're there. Otherwise we move
   * outward until we are an element. If there isn't one we return None
   *
   * Then we move outward to the enclosing element - and if there
   * isn't one we return None. (Which most likely will lead to an SDE.)
   */
  final def enclosingElementCompileInfo: Option[DPathElementCompileInfo] = {
    val eci = this.elementCompileInfo
    eci match {
      case None => None
      case Some(eci) => {
        val eci2 = eci.immediateEnclosingCompileInfo
        eci2 match {
          case None => None
          case Some(ci) => {
            val res = ci.elementCompileInfo
            res
          }
        }
      }
    }
  }

  /**
   * The contract here supports the semantics of "." in paths.
   *
   * If this is an element we're done. If not we move outward
   * until we reach an enclosing element.
   */
  final lazy val elementCompileInfo: Option[DPathElementCompileInfo] = this match {
    case e: DPathElementCompileInfo => Some(e)
    case d: DPathCompileInfo => {
      val eci = d.immediateEnclosingCompileInfo
      eci match {
        case None => None
        case Some(ci) => {
          val res = ci.elementCompileInfo
          res
        }
      }
    }
  }

  /**
   * relative path - relative to enclosing element's path
   * which by definition must be a prefix of this object's path.
   */
  final def relativePath: String = {
    val rel = elementCompileInfo.map {
      parentInfo =>
        Assert.invariant(path.startsWith(parentInfo.path))
        path.substring(parentInfo.path.length)
    }
    val s = rel.getOrElse(path)
    val res = if (s.startsWith("::")) s.substring("::".length) else s
    res
  }
}

/**
 * This class is to contain only things that are needed to do
 * DPath Expression Compilation. Nothing else.
 *
 * This exists because some things have to be compiled (e.g., DPath expressions)
 * which then become part of the runtime data for elements or other.
 *
 * It becomes circular if all the information is bundled together on the
 * RuntimeData or ElementRuntimeData objects. So we split out
 * everything needed to compile expressions will get computed separately
 * (first), and kept on this object, and then subsequently ERD data
 * structures are created which reference these.
 */
class DPathElementCompileInfo(
  @TransientParam parentArg: => Option[DPathCompileInfo],
  @TransientParam variableMap: => VariableMap,
  @TransientParam elementChildrenCompileInfoArg: => Seq[DPathElementCompileInfo],
  namespaces: scala.xml.NamespaceBinding,
  path: String,
  val name: String,
  val isArray: Boolean,
  val namedQName: NamedQName,
  val optPrimType: Option[PrimType],
  sfl: SchemaFileLocation,
  override val tunable: DaffodilTunables)
  extends DPathCompileInfo(parentArg, variableMap, namespaces, path, sfl, tunable)
  with HasSchemaFileLocation {

  lazy val elementChildrenCompileInfo = elementChildrenCompileInfoArg

  override def preSerialization: Any = {
    super.preSerialization
    elementChildrenCompileInfo
  }

  override def toString = "DPathElementCompileInfo(%s)".format(name)

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  final lazy val rootElement: DPathElementCompileInfo =
    this.enclosingElementCompileInfo.map { _.rootElement }.getOrElse { this }

  final def enclosingElementPath: Seq[DPathElementCompileInfo] = {
    enclosingElementCompileInfo match {
      case None => Seq()
      case Some(e) => e.enclosingElementPath :+ e
    }
  }

  /**
   * Finds a child ERD that matches a StepQName. This is for matching up
   * path steps (for example) to their corresponding ERD.
   */
  final def findNamedChild(step: StepQName): DPathElementCompileInfo =
    findNamedMatch(step, elementChildrenCompileInfo)

  final def findRoot(step: StepQName): DPathElementCompileInfo =
    findNamedMatch(step, Seq(this))

  private def findNamedMatch(step: StepQName, possibles: Seq[DPathElementCompileInfo]): DPathElementCompileInfo = {
    val matchesERD: Seq[DPathElementCompileInfo] = step.findMatches(possibles)

    val retryMatchesERD =
      if (matchesERD.isEmpty &&
        tunable.unqualifiedPathStepPolicy == UnqualifiedPathStepPolicy.PreferDefaultNamespace &&
        step.prefix.isEmpty && step.namespace != NoNamespace) {
        // we failed to find a match with the default namespace. Since the
        // default namespace was assumed but didn't match, the unqualified path
        // step policy allows us to try to match NoNamespace elements.
        val noNamespaceStep = step.copy(namespace = NoNamespace)
        noNamespaceStep.findMatches(possibles)
      } else {
        matchesERD
      }

    retryMatchesERD.length match {
      case 0 => noMatchError(step, possibles)
      case 1 => retryMatchesERD(0)
      case _ => {
        // it's ok if all the names are the same, so long as they are
        // in separate choices, or otherwise cannot co-exist.
        //
        // For now we just check if they are siblings, i.e., have the
        // same model group as parent.
        //
        //        val ambiguousSiblings = ambiguousModelGroupSiblings(retryMatchesERD)
        //        if (ambiguousSiblings.isEmpty)
        // BUG
        // This results in multiple siblings with same name, path that is ambiguous
        // just gets first one.
        //
        retryMatchesERD(0)
        //        else
        //          queryMatchError(step, ambiguousSiblings)
      }
    }
  }

  /**
   * Returns a subset of the possibles which are truly ambiguous siblings.
   * Does not find all such, but if any exist, it finds some ambiguous
   * Siblings. Only returns empty Seq if there are no ambiguous siblings.
   */
  //      private def ambiguousModelGroupSiblings(possibles: Seq[DPathElementCompileInfo]) : Seq[DPathElementCompileInfo] = {
  //        val ambiguityLists: Seq[Seq[DPathElementCompileInfo]] = possibles.tails.toSeq.map{
  //          possiblesList =>
  //          if (possiblesList.isEmpty) Nil
  //          else {
  //            val one = possiblesList.head
  //            val rest = possiblesList.tail
  //            val ambiguousSiblings = modelGroupSiblings(one, rest)
  //            val allAmbiguous =
  //                if (ambiguousSiblings.isEmpty) Nil
  //            else one +: ambiguousSiblings
  //            allAmbiguous
  //          }
  //        }
  //          val firstAmbiguous = ambiguityLists
  //          ambiguityLists.head
  //        }

  /**
   * Returns others that are direct siblings of a specific one.
   *
   * Direct siblings means they have the same parent, but that parent
   * cannot be a choice.
   */
  //  private def modelGroupSiblings(one: DPathElementCompileInfo, rest: Seq[DPathElementCompileInfo]): Seq[DPathElementCompileInfo] = {
  //    rest.filter(r => one.immediateEnclosingCompileInfo eq r.immediateEnclosingCompileInfo &&
  //        one.immediateEnclosingCompileInfo
  //  }
  /**
   * Issues a good diagnostic with suggestions about near-misses on names
   * like missing prefixes.
   */
  final def noMatchError(step: StepQName,
    possibles: Seq[DPathElementCompileInfo] = this.elementChildrenCompileInfo) = {
    //
    // didn't find a exact match.
    // So all the rest of this is about providing a meaningful
    // and helpful diagnostic message.
    //
    // Did the local name match at all?
    //
    val localOnlyERDMatches = {
      val localName = step.local
      if (step.namespace == NoNamespace) Nil
      else possibles.map { _.namedQName }.collect {
        case localMatch if localMatch.local == localName => localMatch
      }
    }
    //
    // If the local name matched, then perhaps the user just forgot
    // to put on a prefix.
    //
    // We want to suggest use of a prefix that is bound to the
    // desired namespace already.. that is from within our current scope
    //
    val withStepsQNamePrefixes =
      localOnlyERDMatches.map { qn =>
        val stepPrefixForNS = NS.allPrefixes(qn.namespace, this.namespaces)
        val proposedStep = stepPrefixForNS match {
          case Nil => qn
          case Seq(hd, _*) => StepQName(Some(hd), qn.local, qn.namespace)
        }
        proposedStep
      }
    val interestingCandidates = withStepsQNamePrefixes.map { _.toPrettyString }.mkString(", ")
    if (interestingCandidates.length > 0) {
      SDE("No element corresponding to step %s found,\nbut elements with the same local name were found (%s).\nPerhaps a prefix is incorrect or missing on the step name?",
        step.toPrettyString, interestingCandidates)
    } else {
      //
      // There weren't even any local name matches.
      //
      val interestingCandidates = possibles.map { _.namedQName }.mkString(", ")
      if (interestingCandidates != "")
        SDE("No element corresponding to step %s found. Possibilities for this step include: %s.",
          step.toPrettyString, interestingCandidates)
      else
        SDE("No element corresponding to step %s found.",
          step.toPrettyString)
    }
  }

  //  private def queryMatchError(step: StepQName, matches: Seq[DPathElementCompileInfo]) = {
  //    SDE("Statically ambiguous or query-style paths not supported in step path: '%s'. Matches are at locations:\n%s",
  //      step, matches.map(_.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
  //  }
}
