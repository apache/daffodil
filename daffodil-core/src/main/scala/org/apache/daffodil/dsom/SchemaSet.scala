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

package org.apache.daffodil.dsom

import scala.xml.Node
import org.apache.daffodil.externalvars.{ Binding, BindingException }
import org.apache.daffodil.compiler.RootSpec
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.xml._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.NS
import org.apache.daffodil.oolag.OOLAG
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.grammar.primitives.VariableMapFactory
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.dpath.NodeInfo
import java.io.File
import org.apache.daffodil.xml.DFDLCatalogResolver
import org.apache.daffodil.api.DaffodilSchemaSource
import org.apache.daffodil.api.UnitTestSchemaSource
import org.apache.daffodil.schema.annotation.props.LookupLocation
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.processors.TypeCalculator
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import org.apache.daffodil.compiler.ProcessorFactory

/**
 * A schema set is exactly that, a set of schemas. Each schema has
 * a target namespace (or 'no namespace'), so a schema set is
 * conceptually a mapping from a namespace URI (or empty string, meaning no
 * namespace) onto schema.
 *
 * Constructing these from XML Nodes is a unit-test
 * interface. The real constructor takes a sequence of file names,
 * and you can optionally specify a root element via the rootSpec argument.
 *
 * A schema set is a SchemaComponent (derived from that base), so as to inherit
 * the error/warning accumulation behavior that all SchemaComponents share.
 * A schema set invokes our XML Loader, which can produce validation errors, and
 * those have to be gathered so we can give the user back a group of them, not
 * just one.
 *
 * Schema set is however, a kind of a fake SchemaComponent in that it
 * doesn't correspond to any user-specified schema object. And unlike other
 * schema components obviously it does not live within a schema document.
 */

final class SchemaSet(
  pfArg: => Option[ProcessorFactory],
  rootSpec: Option[RootSpec],
  externalVariables: Seq[Binding],
  schemaSourcesArg: Seq[DaffodilSchemaSource],
  val validateDFDLSchemas: Boolean,
  checkAllTopLevelArg: Boolean,
  tunableArg: DaffodilTunables)
  extends SchemaComponentImpl(<schemaSet/>, None)
  with SchemaSetIncludesAndImportsMixin {

  private lazy val processorFactory = pfArg // insure this by name arg is evaluated exactly once.

  lazy val root = rootElement(processorFactory.flatMap { _.rootSpec })

  override lazy val tunable =
    tunableArg

  requiredEvaluations(isValid)
  requiredEvaluations(typeCalcMap)

  if (checkAllTopLevel) {
    requiredEvaluations(checkForDuplicateTopLevels())
    requiredEvaluations(this.allTopLevels)
  }
  requiredEvaluations(variableMap)

  lazy val resolver = DFDLCatalogResolver.get

  override lazy val schemaSet = this
  // These things are needed to satisfy the contract of being a schema component.
  final override protected def enclosingComponentDef = None
  override lazy val schemaDocument = Assert.usageError("schemaDocument should not be called on SchemaSet")

  override lazy val lineAttribute: Option[String] = None

  lazy val schemaSources = schemaSourcesArg

  /**
   * Let's use the uri for the first schema document, rather than giving no information at all.
   *
   * It would appear that this is only used for informational purposes
   * and as such, doesn't need to be a URL.  Can just be String.
   */
  override lazy val uriString: String = schemaSources(0).uriForLoading.toString

  lazy val checkAllTopLevel = checkAllTopLevelArg

  override def warn(th: Diagnostic) = oolagWarn(th)
  override def error(th: Diagnostic) = oolagError(th)

  /**
   * This constructor for unit testing only
   */
  def this(sch: Node, rootNamespace: String = null, root: String = null, extVars: Seq[Binding] = Seq.empty, optTmpDir: Option[File] = None, tunableOpt: Option[DaffodilTunables] = None) =
    this(
      None,
      {
        if (root == null) None else {
          if (rootNamespace == null) Some(RootSpec(None, root))
          else Some(RootSpec(Some(NS(rootNamespace)), root))
        }
      },
      extVars,
      List(UnitTestSchemaSource(sch, Option(root).getOrElse("anon"), optTmpDir)),
      false,
      false,
      tunableOpt.getOrElse(DaffodilTunables()))

  lazy val schemaFileList = schemas.map(s => s.uriString)

  lazy val isValid = {
    val isV = OOLAG.keepGoing(false) {
      val files = allSchemaFiles
      val fileValids = files.map { _.isValid }
      val res = fileValids.length > 0 && fileValids.fold(true) { _ && _ }
      res
    }
    isV
  }

  lazy val validationDiagnostics = {
    val files = allSchemaFiles
    val res = files.flatMap { _.validationDiagnostics }
    res
  }

  lazy val schemas: Seq[Schema] = LV('schemas) {
    val schemaPairs = allSchemaDocuments.map { sd => (sd.targetNamespace, sd) }
    //
    // groupBy is deterministic if the hashCode of the key element is deterministic.
    // our NS objects hashCode is same as their underlying string.
    //
    // Alas, being deterministic doesn't mean it is in an order we expect.
    // but at least it is deterministic.
    val schemaGroups = schemaPairs.groupBy { _._1 } // group by the namespace identifier
    val schemas = schemaGroups.map {
      case (ns, pairs) => {
        val sds = pairs.map { case (ns, s) => s }
        val sch = new Schema(ns, sds.toSeq, this)
        sch
      }
    }
    schemas.toSeq
  }.value

  lazy val globalSimpleTypeDefs: Seq[GlobalSimpleTypeDef] = schemas.flatMap(_.globalSimpleTypeDefs)

  lazy val typeCalcMap: Map[GlobalQName, TypeCalculator[AnyRef, AnyRef]] = {
    val factories = globalSimpleTypeDefs
    val withCalc = factories.filter(_.optTypeCalculator.isDefined)
    val mappings = withCalc.map(st => (st.globalQName, st.optTypeCalculator.get))

    mappings.toMap
  }

  /**
   * For checking uniqueness of global definitions in their namespaces
   */

  private type UC = (NS, String, Symbol, GlobalComponent)

  private def allTopLevels: Seq[UC] = LV('allTopLevels) {
    val res = schemas.flatMap { schema =>
      {
        val ns = schema.namespace
        val geds = schema.globalElementDecls.map { g =>
          {
            (ns, g.name, 'Element, g)
          }
        }
        val stds = schema.globalSimpleTypeDefs.map { g =>
          {
            (ns, g.name, 'SimpleType, g)
          }
        }
        val ctds = schema.globalComplexTypeDefs.map { g =>
          {
            (ns, g.name, 'ComplexType, g)
          }
        }
        val gds = schema.globalGroupDefs.map { g =>
          {
            (ns, g.name, 'Group, g)
          }
        }
        val dfs = schema.defineFormats.map { g =>
          {
            (ns, g.name, 'DefineFormat, g)
          }
        }
        val dess = schema.defineEscapeSchemes.map { g =>
          {
            (ns, g.name, 'DefineEscapeScheme, g)
          }
        }
        val dvs = schema.defineVariables.map { g =>
          {
            (ns, g.name, 'DefineVariable, g)
          }
        }
        val all = geds ++ stds ++ ctds ++ gds ++ dfs ++ dess ++ dvs
        all
      }
    }
    res.asInstanceOf[Seq[UC]]
  }.value

  private def groupedTopLevels = LV('groupedTopLevels) {
    val grouped = allTopLevels.groupBy {
      case (ns, name, kind, obj) => {
        (kind, ns, name)
      }
    }
    val grouped2 = grouped.map {
      case (idFields, seq) => {
        val onlyObj = seq.map { case (ns, name, kind, obj) => obj }
        if (onlyObj.length > 1) {
          val (ns, name, kind) = idFields
          val locations = onlyObj.asInstanceOf[Seq[LookupLocation]] // don't like this downcast
          SDEButContinue("multiple definitions for %s  {%s}%s.\n%s", kind.toString, ns, name,
            locations.map { _.locationDescription }.mkString("\n"))
        }
        (idFields, onlyObj)
      }
    }
    val res = grouped2.flatMap { case (_, topLevelThing) => topLevelThing }.toSeq
    res
  }.value

  // The trick with this is when to call it. If you call it, as
  // a consequence of computing all of this, it will have to parse
  // every file, every included/imported file, etc.
  def checkForDuplicateTopLevels() = {
    groupedTopLevels // demand this.
  }

  /**
   * When the user (of the API) doesn't specify a root element namespace, just a
   * root element name, then this searches for a single element having that name, and if it is
   * unambiguous, it is used as the root.
   */
  private def findRootElement(name: String) = {
    // log(Info("%s searching for root element with name %s", Misc.getNameFromClass(this), name))
    val candidates = schemas.flatMap { _.getGlobalElementDecl(name) }
    schemaDefinitionUnless(candidates.length != 0, "No root element found for %s in any available namespace", name)
    schemaDefinitionUnless(candidates.length <= 1, "Root element %s is ambiguous. Candidates are %s.",
      candidates.map { gef =>
        {
          val tns = gef.schemaDocument.targetNamespace
          Assert.invariant(!tns.isUnspecified)
          gef.name + " " + tns.explainForMsg
        }
      })
    Assert.invariant(candidates.length == 1)
    val gef = candidates(0)
    val re = gef.forRoot()
    re
  }

  /**
   * Given a RootSpec, get the global element it specifies. Error if ambiguous
   * or not found.
   */
  private def getGlobalElement(rootSpec: RootSpec) = {
    rootSpec match {
      case RootSpec(Some(rootNamespaceName), rootElementName) => {
        val qn = RefQName(None, rootElementName, rootNamespaceName)
        val geFactory = getGlobalElementDecl(qn)
        val ge = geFactory match {
          case None => schemaDefinitionError("No global element found for %s", rootSpec)
          case Some(f) => f.forRoot()
        }
        ge
      }
      case RootSpec(None, rootElementName) => {
        findRootElement(rootElementName)
      }
      case _ => Assert.impossible()
    }
  }

  /**
   * Since the root element can be specified by an API call on the
   * Compiler class, or by an API call on the ProcessorFactory, this
   * method reconciles the two. E.g., you can't specify the root both
   * places, it's one or the other.
   *
   * Also, if you don't specify a root element at all, this
   * grabs the first element declaration of the first schema file
   * to use as the root.
   */
  def rootElement(rootSpecFromProcessorFactory: Option[RootSpec]): Root = {
    val rootSpecFromCompiler = rootSpec
    val re =
      (rootSpecFromCompiler, rootSpecFromProcessorFactory) match {
        case (Some(rs), None) =>
          getGlobalElement(rs)

        case (None, Some(rs)) =>
          getGlobalElement(rs)

        case (None, None) => {
          // if the root element and rootNamespace aren't provided at all, then
          // the first element of the first schema document is the root
          val sDocs = this.allSchemaDocuments
          assuming(sDocs.length > 0)
          val firstSchemaDocument = sDocs(0)
          val gdeclf = firstSchemaDocument.globalElementDecls
          val firstElement = {
            schemaDefinitionUnless(gdeclf.length >= 1, "No global elements in: " + firstSchemaDocument.uriString)
            val root = gdeclf(0).forRoot()
            root
          }
          firstElement
        }
        case _ => Assert.invariantFailed("illegal combination of root element specifications")
      }
    re
  }

  /**
   * Retrieve schema by namespace name.
   *
   * If the schema has no namespace, then use ""
   */
  def getSchema(namespace: NS) = {
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }

  /**
   * XML Schema global objects.
   * Given a namespace and name, try to retrieve the named object
   *
   * These all return factories for the objects, not the objects themselves.
   */
  def getGlobalElementDecl(refQName: RefQName) = {
    val s = getSchema(refQName.namespace)
    val res = s.flatMap { s =>
      {
        val ged = s.getGlobalElementDecl(refQName.local)
        ged
      }
    }
    res
  }
  def getGlobalSimpleTypeDef(refQName: RefQName) = getSchema(refQName.namespace).flatMap { _.getGlobalSimpleTypeDef(refQName.local) }
  def getGlobalComplexTypeDef(refQName: RefQName) = getSchema(refQName.namespace).flatMap { _.getGlobalComplexTypeDef(refQName.local) }
  def getGlobalGroupDef(refQName: RefQName) = getSchema(refQName.namespace).flatMap { _.getGlobalGroupDef(refQName.local) }

  /**
   * DFDL Schema top-level global objects
   */
  def getDefineFormat(refQName: RefQName) = {
    val s = getSchema(refQName.namespace)
    s.flatMap { _.getDefineFormat(refQName.local) }
  }
  def getDefineFormats(namespace: NS, context: ThrowsSDE) = getSchema(namespace) match {
    case None => context.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(refQName: RefQName) = {
    val res = getSchema(refQName.namespace).flatMap { _.getDefineVariable(refQName.local) }
    val finalResult = res match {
      case None => {
        val optRes = this.predefinedVars.find(dfv => {
          dfv.namespace == refQName.namespace && dfv.name == refQName.local
        })
        optRes
      }
      case Some(value) => res
    }
    finalResult
  }
  def getDefineEscapeScheme(refQName: RefQName) = getSchema(refQName.namespace).flatMap { _.getDefineEscapeScheme(refQName.local) }

  def getPrimitiveType(refQName: RefQName) = {
    if (refQName.namespace != XMLUtils.XSD_NAMESPACE) // must check namespace
      None
    else {
      val optPrimNode = NodeInfo.PrimType.fromNameString(refQName.local)
      optPrimNode.map { PrimitiveType(_) }
    }
  }

  /**
   * Creates a DFDLDefineVariable object for the predefined variable.
   *
   * @param theName The variable name.
   * @param theType The type of the variable. ex. xs:string
   * @param nsURI The namespace URI of the variable.
   *
   * @return A Seq[DFDLDefineVariable]
   */
  private def generateDefineVariable(theName: String, theType: String, theDefaultValue: String, nsURI: String, sdoc: SchemaDocument) = {
    val dfv = new DFDLDefineVariable(
      <dfdl:defineVariable name={ theName } type={ theType } defaultValue={ theDefaultValue } external="true" xmlns:xs={ XMLUtils.XSD_NAMESPACE.toString }/>, sdoc) {
      override lazy val namespace = NS(nsURI)
      override lazy val targetNamespace = NS(nsURI)
    }
    dfv
  }

  private lazy val schemaDocForGlobalVars = {
    //
    // OOLAG no longer catches broad classes of exceptions like index out of bounds
    //
    // This avoids OOLAG masking what are coding errors and disguising them as some sort of
    // error in the DFDL schema; however, it also requires that attributes that are
    // evaluated to determine if there is an error in an object (and to force gathering of diagnostics)
    // cannot assume that other data structures are correct.
    //
    // In this case, if the schema document isn't valid, then there won't even be
    // any schemas or schemaDocuments, so we'll index-out-of-bounds, and OOLAG
    // won't suppress that. So we code defensively.
    //
    assuming(schemas.length > 0)
    assuming(schemas(0).schemaDocuments.length > 0)
    this.schemas(0).schemaDocuments(0)
  }

  // We'll declare these here at the SchemaSet level since they're global.
  lazy val predefinedVars = {
    val nsURI = XMLUtils.DFDL_NAMESPACE.toStringOrNullIfNoNS

    val encDFV = generateDefineVariable("encoding", "xs:string", "UTF-8", nsURI, schemaDocForGlobalVars)
    val boDFV = generateDefineVariable("byteOrder", "xs:string", "bigEndian", nsURI, schemaDocForGlobalVars)
    val binDFV = generateDefineVariable("binaryFloatRep", "xs:string", "ieee", nsURI, schemaDocForGlobalVars)
    val outDFV = generateDefineVariable("outputNewLine", "xs:string", "%LF;", nsURI, schemaDocForGlobalVars)

    Seq(encDFV, boDFV, binDFV, outDFV)
  }

  /**
   * Determines if any of the externally defined variables
   * were specified expecting Daffodil to figure out the
   * namespace.  If so, Daffodil attempts to guess the
   * namespace and will SDE if there is any ambiguity.
   *
   * @param allDefinedVariables The list of all DFDLDefineVariables in the SchemaSet.
   *
   * @return A list of external variables updated with any found namespaces.
   */
  private def resolveExternalVariableNamespaces(allDefinedVariables: Seq[DFDLDefineVariable]) = {
    val finalExternalVariables: scala.collection.mutable.Queue[Binding] = scala.collection.mutable.Queue.empty

    val extVarsWithoutNS = externalVariables.filterNot(b => b.hasNamespaceSpecified)

    val extVarsWithNS = externalVariables.filter(b => b.hasNamespaceSpecified)

    extVarsWithNS.foreach(b => finalExternalVariables.enqueue(b))

    extVarsWithoutNS.foreach(v => {
      Assert.invariant(v.varQName.namespace.isUnspecified)
      val matchingDVs = allDefinedVariables.filter { dv =>
        // just compare local names. We're searching for an unambiguous match
        v.varQName.local == dv.namedQName.local
      }

      matchingDVs.length match {
        case 0 => this.SDE("Could not find the externally defined variable %s.", v.varQName)
        case x: Int if x > 1 =>
          this.SDE(
            "The externally defined variable %s is ambiguous.  " +
              "A namespace is required to resolve the ambiguity.\nFound:\t%s",
            v.varQName, matchingDVs.mkString(", "))
        case _ => // This is OK, we have exactly 1 match
      }

      val newNS = matchingDVs.head.namespace
      val newBinding = try {
        Binding(v.varQName.local, Some(newNS), v.varValue)
      } catch {
        case e: BindingException => this.SDE("Exception when processing external variable %s: %s", v.varQName, e.getMessage)
      }
      finalExternalVariables.enqueue(newBinding)
    })
    finalExternalVariables
  }

  override def variableMap = LV('variableMap) {
    val dvs = allSchemaDocuments.flatMap { _.defineVariables }
    val alldvs = dvs.union(predefinedVars)
    val vmap = VariableMapFactory.create(alldvs)

    // At this point we want to try to figure out which, if any, external
    // variables did not have a namespace specified.
    val finalExternalVariables = resolveExternalVariableNamespaces(alldvs)

    val finalVMap =
      ExternalVariablesLoader.loadVariables(finalExternalVariables, this, vmap)

    finalVMap
  }.value
}

class ValidateSchemasErrorHandler(sset: SchemaSet) extends org.xml.sax.ErrorHandler {

  def warning(exception: org.xml.sax.SAXParseException) = {
    val sdw = new SchemaDefinitionWarning(sset.schemaFileLocation, "Warning loading schema due to %s", exception)
    sset.warn(sdw)
  }

  def error(exception: org.xml.sax.SAXParseException) = {
    val sde = new SchemaDefinitionError(sset.schemaFileLocation, "Error loading schema due to %s", exception)
    sset.error(sde)
  }

  def fatalError(exception: org.xml.sax.SAXParseException) = this.error(exception)
}
