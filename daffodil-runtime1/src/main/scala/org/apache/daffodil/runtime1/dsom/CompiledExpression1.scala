/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.dsom

import scala.runtime.ScalaRunTime.stringOf

import org.apache.daffodil.lib.api.UnqualifiedPathStepPolicy
import org.apache.daffodil.lib.api.WarnID
import org.apache.daffodil.lib.exceptions.HasSchemaFileLocation
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.PreSerialization
import org.apache.daffodil.lib.util.TransientParam
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.NoNamespace
import org.apache.daffodil.lib.xml.StepQName
import org.apache.daffodil.runtime1.dpath.DState
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.VariableMap

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
abstract class CompiledExpression[+T <: AnyRef](val qName: NamedQName, value: AnyRef)
  extends ContentValueReferencedElementInfoMixin
  with Serializable {

  DataValue.assertValueIsNotDataValue(value)

  final def toBriefXML(depth: Int = -1) = {
    "'" + prettyExpr + "'"
  }

  /**
   * Note use of the `stringOf(v)` below.
   * Turns out `x.toString` creates some crappy printed representations,
   * particularly for `Array[Byte]`. It prints a useless thing like "[@0909280".
   * Use of `stringOf` prints "Array(....)".
   */
  lazy val prettyExpr = stringOf(value)

  /**
   * Tells us if the expression is the constant empty string (that is, it is "").
   */
  final lazy val isConstantEmptyString = value == ""

  /**
   * Tells us if the expression can match the empty string. We know it can if the expression
   * is a DFDL entity like %ES; or %WSP*. We do not know whether it can if it is a more
   * complicated constant or runtime expression.
   */
  final lazy val isKnownCanMatchEmptyString = value == "%ES;" || value == "%WSP*;"

  /**
   * used to obtain a constant value.
   *
   * isConstant must be true or this will throw.
   */
  def isConstant: Boolean

  def evaluate(state: ParseOrUnparseState): T

  def run(dstate: DState): Unit

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
  def evaluateForwardReferencing(
    state: ParseOrUnparseState,
    whereBlockedLocation: Suspension,
  ): Maybe[T]

  override def toString(): String = "CompiledExpression(" + value.toString + ")"

}

object ReferencedElementInfos {

  val None = Set.empty.asInstanceOf[Set[DPathElementCompileInfo]]

}

final case class ConstantExpression[+T <: AnyRef](qn: NamedQName, kind: NodeInfo.Kind, value: T)
  extends CompiledExpression[T](qn, value) {

  def targetType = kind

  lazy val sourceType: NodeInfo.Kind = NodeInfo.fromObject(value)

  override def evaluate(state: ParseOrUnparseState) = value

  override def run(dstate: DState) = dstate.setCurrentValue(DataValue.unsafeFromAnyRef(value))

  final def evaluateForwardReferencing(
    state: ParseOrUnparseState,
    whereBlockedLocation: Suspension,
  ): Maybe[T] = {
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
  // parentsDelay is a transient due to serialization order issues causing
  // stack overflows. There is no delay/lazy/by-name involvement here. See the
  // lazy val parents scaladoc for a detailed explanation.
  @TransientParam parentsDelay: Delay[Seq[DPathCompileInfo]],
  val variableMap: VariableMap,
  val namespaces: scala.xml.NamespaceBinding,
  val path: String,
  override val schemaFileLocation: SchemaFileLocation,
  val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy,
) extends ImplementsThrowsSDE
  with PreSerialization
  with HasSchemaFileLocation {

  def initialize: Unit = {
    parents
  }

  /**
   * This "parents" and "parentsDelay" variables are backpointers to all
   * DPathCompileInfo's that reference this DPathCompileInfo. The problem with
   * this is that when elements are shared, these backpointers create a highly
   * connected and cyclic graph that requires a large stack to serialize using
   * default Java serialization as it jumps around parents and children. To
   * avoid this large stack requirement, we mark the parents and parentsDelay
   * backpointers as transient. This prevents Java from serializing these
   * backpointers at all, turning this into serialization of a directed
   * acycling graph, which only requires a stack depth relative to the schema
   * depth. Once all that default serialization is completed and all the
   * DPathCompileInfo's are serialized, we then manually traverse all the
   * DPathCompileInfo's again and serialize the parent sequences (via the
   * serailizeParents method). Because all the DPathCompileInfo's are already
   * serialized, the seralizeParents is really just serializing a Seq with
   * pointers to objects that have already been serialized, which avoids the
   * serialization cycles.
   */
  @transient lazy val parents = parentsDelay.value

  def serializeParents(oos: java.io.ObjectOutputStream): Unit = {
    oos.writeObject(parents)
  }

  def deserializeParents(ois: java.io.ObjectInputStream): Unit = {
    val deserializedParents = ois.readObject().asInstanceOf[Seq[DPathCompileInfo]]

    // Set the parents field via reflection so that it can be a val rather than a var
    val clazz = this.getClass
    val parentsField =
      try {
        clazz.getDeclaredField("parents")
      } catch {
        case e: java.lang.NoSuchFieldException =>
          clazz.getSuperclass.getDeclaredField("parents")
      }
    parentsField.setAccessible(true)
    parentsField.set(this, deserializedParents) // set the value to the deserialized value
    parentsField.setAccessible(false)
  }

  def diagnosticDebugName = path

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  override def toString = "DPathCompileInfo(%s)".format(path)

  /**
   * The contract here supports the semantics of ".." in paths.
   *
   * First we establish the invariant of being on an element. If the
   * schema component is an element we're there. Otherwise we move
   * outward until we are an element. If there isn't one we return None
   *
   * Then we move outward to the enclosing element - and if there
   * isn't one we return None. (Which most likely will lead to an SDE.)
   *
   * The map(identity) is used work around a bug related to serialization of Lists
   * and the SerializationProxy object.
   */
  final lazy val enclosingElementCompileInfos: Seq[DPathElementCompileInfo] = {
    val eci = elementCompileInfos.flatMap {
      _.parents
    }
    val res = eci.flatMap {
      _.elementCompileInfos
    }
    res
  }.map(identity)

  /**
   * The contract here supports the semantics of "." in paths.
   *
   * If this is an element we're done. If not we move outward
   * until we reach an enclosing element.
   *
   * This is used because paths refer to elements, so we have to
   * walk upward until we get elements. At that point we can
   * then navigate element to element.
   *
   * The map(identity) is used work around a bug related to serialization of
   * Lists and the SerializationProxy object.
   */
  final lazy val elementCompileInfos: Seq[DPathElementCompileInfo] = {
    this match {
      case e: DPathElementCompileInfo => Seq(e)
      case d: DPathCompileInfo => {
        val eci = d.parents
        eci.flatMap { ci => ci.elementCompileInfos }
      }
    }
  }.map(identity)
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
  // parentsDelay is a transient due to serialization order issues causing
  // stack overflows. There is no delay/lazy/by-name involvement here. See the
  // lazy val parents scaladoc for a detailed explanation.
  @TransientParam parentsDelay: Delay[Seq[DPathElementCompileInfo]],
  variableMap: VariableMap,
  // This next arg must be a Delay as we're creating a circular
  // structure here. Element's compile info points down to their children. Children
  // point back to their parents, which may be multiple parents if they are shared.
  //
  // We choose a Delay object not call-by-name because it has features to enable
  // the GC to collect the closure objects that are created from the calling context
  // to realize the by-name arg expression.
  elementChildrenCompileInfoDelay: Delay[Seq[DPathElementCompileInfo]],
  namespaces: scala.xml.NamespaceBinding,
  path: String,
  val name: String,
  val isArray: Boolean,
  val namedQName: NamedQName,
  val optPrimType: Option[PrimType],
  sfl: SchemaFileLocation,
  override val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy,
  val sscd: String,
  val isOutputValueCalc: Boolean,
  val isDistinguishedRoot: Boolean,
) extends DPathCompileInfo(
    parentsDelay.asInstanceOf[Delay[Seq[DPathCompileInfo]]],
    variableMap,
    namespaces,
    path,
    sfl,
    unqualifiedPathStepPolicy,
  ) {

  /**
   * Cyclic objects require initialization
   */
  override lazy val initialize: Unit = {
    parents
    elementChildrenCompileInfo
  }

  override def serializeParents(oos: java.io.ObjectOutputStream): Unit = {
    super.serializeParents(oos)
    elementChildrenCompileInfo.foreach { _.serializeParents(oos) }
  }

  override def deserializeParents(ois: java.io.ObjectInputStream): Unit = {
    super.deserializeParents(ois)
    elementChildrenCompileInfo.foreach { _.deserializeParents(ois) }
  }

  lazy val elementChildrenCompileInfo = elementChildrenCompileInfoDelay.value

  override def preSerialization: Any = {
    super.preSerialization
    elementChildrenCompileInfo
  }

  final def typeNode: NodeInfo.Kind =
    if (optPrimType.isDefined) optPrimType.get
    else NodeInfo.Complex

  /**
   * Stores whether or not this element is used in any path step expressions
   * during schema compilation. Note that this needs to be a var since its
   * value is determined during DPath compilation, which requires that the
   * DPathElementCompileInfo already exists. So this must be a mutable value
   * that can be flipped during schema compilation.
   *
   * Note that in the case of multiple child element decls with the same name,
   * we must make sure ALL of them get this var set.
   *
   * This is done on the Seq returned when findNameMatches is called.
   */
  var isReferencedByExpressions = false

  override def toString = "DPathElementCompileInfo(%s)".format(name)

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  final lazy val rootElement: DPathElementCompileInfo =
    if (elementCompileInfos.isEmpty) this
    else if (enclosingElementCompileInfos.isEmpty) this
    else
      enclosingElementCompileInfos.head.rootElement

  /**
   * Marks compile info that element is referenced by an expression    //
   *
   * We must indicate for all children having this path step as their name
   * that they are referenced by expression. Expressions that end in such
   * a path step are considered "query style" expressions as they may
   * return more than one node, which DFDL v1.0 doesn't allow. (They also may
   * not return multiple, as the different path step children could be in
   * different choice branches. Either way, we have to indicate that they are
   * ALL referenced by this path step.
   */
  def indicateReferencedByExpression(matches: Seq[DPathElementCompileInfo]): Unit = {
    matches.foreach { info =>
      info.isReferencedByExpressions = true
    }
  }

  /**
   * Finds a child ERD that matches a StepQName. This is for matching up
   * path steps (for example) to their corresponding ERD.
   *
   * TODO: Must eventually change to support query-style expressions where there
   * can be more than one such child.
   */
  final def findNamedChild(
    step: StepQName,
    expr: ImplementsThrowsOrSavesSDE,
  ): DPathElementCompileInfo = {
    val matches = findNamedMatches(step, elementChildrenCompileInfo, expr)
    indicateReferencedByExpression(matches)
    matches(0)
  }

  final def findRoot(
    step: StepQName,
    expr: ImplementsThrowsOrSavesSDE,
  ): DPathElementCompileInfo = {
    val matches = findNamedMatches(step, Seq(this), expr)
    indicateReferencedByExpression(matches)
    matches(0)
  }

  private def findNamedMatches(
    step: StepQName,
    possibles: Seq[DPathElementCompileInfo],
    expr: ImplementsThrowsOrSavesSDE,
  ): Seq[DPathElementCompileInfo] = {
    val matchesERD: Seq[DPathElementCompileInfo] = step.findMatches(possibles)

    val retryMatchesERD =
      if (
        matchesERD.isEmpty &&
        unqualifiedPathStepPolicy == UnqualifiedPathStepPolicy.PreferDefaultNamespace &&
        step.prefix.isEmpty && step.namespace != NoNamespace
      ) {
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
      case 1 => // ok
      case _ => queryMatchWarning(step, retryMatchesERD, expr)
    }
    retryMatchesERD
  }

  final def findNamedChildren(
    step: StepQName,
    possibles: Seq[DPathElementCompileInfo],
  ): Seq[DPathElementCompileInfo] = {
    val matchesERD = step.findMatches(possibles)
    val retryMatchesERD =
      if (
        matchesERD.isEmpty &&
        unqualifiedPathStepPolicy == UnqualifiedPathStepPolicy.PreferDefaultNamespace &&
        step.prefix.isEmpty && step.namespace != NoNamespace
      ) {
        // we failed to find a match with the default namespace. Since the
        // default namespace was assumed but didn't match, the unqualified path
        // step policy allows us to try to match NoNamespace elements.
        val noNamespaceStep = step.copy(namespace = NoNamespace)
        noNamespaceStep.findMatches(possibles)
      } else {
        matchesERD
      }
    indicateReferencedByExpression(retryMatchesERD)
    retryMatchesERD
  }

  /**
   * Issues a good diagnostic with suggestions about near-misses on names
   * like missing prefixes.
   */
  final def noMatchError(
    step: StepQName,
    possibles: Seq[DPathElementCompileInfo] = this.elementChildrenCompileInfo,
  ) = {
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
      else
        possibles.map { _.namedQName }.collect {
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
      SDE(
        "No element corresponding to step %s found,\nbut elements with the same local name were found (%s).\nPerhaps a prefix is incorrect or missing on the step name?",
        step.toPrettyString,
        interestingCandidates,
      )
    } else {
      //
      // There weren't even any local name matches.
      //
      val interestingCandidates = possibles.map { _.namedQName }.mkString(", ")
      if (interestingCandidates != "")
        SDE(
          "No element corresponding to step %s found. Possibilities for this step include: %s.",
          step.toPrettyString,
          interestingCandidates,
        )
      else
        SDE("No element corresponding to step %s found.", step.toPrettyString)
    }
  }

  final def queryMatchWarning(
    step: StepQName,
    matches: Seq[DPathElementCompileInfo],
    expr: ImplementsThrowsOrSavesSDE,
  ) = {
    expr.SDW(
      WarnID.QueryStylePathExpression,
      "Statically ambiguous or query-style paths not supported in step path: '%s'. Matches are at locations:\n%s",
      step,
      matches.map(_.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""),
    )
  }
}
