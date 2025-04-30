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

package org.apache.daffodil.core.dsom

import java.io.File
import scala.xml.Node

import org.apache.daffodil.core.compiler.RootSpec
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.SchemaSetGrammarMixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.UnitTestSchemaSource
import org.apache.daffodil.lib.oolag.OOLAG
import org.apache.daffodil.lib.schema.annotation.props.LookupLocation
import org.apache.daffodil.lib.util.TransitiveClosure
import org.apache.daffodil.lib.xml._

object SchemaSet {
  def apply(
    optPFRootSpec: Option[RootSpec],
    schemaSource: DaffodilSchemaSource,
    shouldValidateDFDLSchemas: Boolean,
    checkAllTopLevel: Boolean,
    tunables: DaffodilTunables
  ) = {
    val ss = new SchemaSet(
      optPFRootSpec,
      schemaSource,
      shouldValidateDFDLSchemas,
      checkAllTopLevel,
      tunables
    )
    ss.initialize()
    ss
  }

  def apply(
    sch: Node,
    rootNamespace: String = null,
    root: String = null,
    optTmpDir: Option[File] = None,
    tunableOpt: Option[DaffodilTunables] = None
  ) = {
    val ss = new SchemaSet(sch, rootNamespace, root, optTmpDir, tunableOpt)
    ss.initialize()
    ss
  }
}

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

final class SchemaSet private (
  optPFRootSpec: Option[RootSpec],
  val schemaSource: DaffodilSchemaSource,
  val shouldValidateDFDLSchemas: Boolean,
  val checkAllTopLevel: Boolean,
  val tunables: DaffodilTunables
) extends SchemaComponentImpl(<schemaSet/>, None)
  with SchemaSetIncludesAndImportsMixin
  with SchemaSetGrammarMixin {

  override protected[dsom] def initialize(): Unit = {
    // no normal schema component initialization for SchemaSet
  }

  /**
   * Used to count instances of element base objects created by
   * compiler - to be sure we're not duplicating them unnecessarily.
   */
  var elementBaseInstanceCount = 0L

  lazy val sharedComplexContentFactory = new SharedFactory[Gram]
  lazy val sharedNilLitFactory = new SharedFactory[Gram]
  lazy val sharedSimpleValueFactory = new SharedFactory[Gram]
  lazy val sharedGroupContentsFactory = new SharedFactory[Gram]
  lazy val sharedGroupMembersFactory = new SharedFactory[Seq[Term]]

  override lazy val tunable = tunables

  requiredEvaluationsAlways(root)
  requiredEvaluationsAlways(checkForDuplicateTopLevels())

  lazy val resolver = DFDLCatalogResolver.get

  override lazy val optSchemaDocument = None
  override lazy val optXMLSchemaDocument = None

  override lazy val lineAttribute: Option[String] = None

  /**
   * Let's use the uri for the first schema document, rather than giving no information at all.
   *
   * It would appear that this is only used for informational purposes
   * and as such, doesn't need to be a URL.  Can just be String.
   */
  override lazy val uriString: String = schemaSource.uriForLoading.toString

  override lazy val diagnosticFile: File = schemaSource.diagnosticFile

  override def warn(th: Diagnostic) = oolagWarn(th)

  override def error(th: Diagnostic) = oolagError(th)

  /**
   * This constructor for unit testing only
   */
  private def this(
    sch: Node,
    rootNamespace: String = null,
    root: String = null,
    optTmpDir: Option[File] = None,
    tunableOpt: Option[DaffodilTunables] = None
  ) =
    this(
      {
        if (root == null) None
        else {
          if (rootNamespace == null) Some(RootSpec(None, root))
          else Some(RootSpec(Some(NS(rootNamespace)), root))
        }
      },
      UnitTestSchemaSource(sch, Option(root).getOrElse("anon"), optTmpDir),
      false,
      false,
      tunableOpt.getOrElse(DaffodilTunables())
    )

  lazy val schemaFileList = schemas.map(s => s.uriString)

  private lazy val isValid: Boolean = {
    if (!shouldValidateDFDLSchemas)
      true // pretend it's valid though for some specific tests it may not be
    else {
      //
      // We use keepGoing here, because we want to gather a validation error,
      // suppress further propagation of it, and return false.
      //
      val isEachFileIndividuallyValid = OOLAG.keepGoing(false) {
        val files = allSchemaFiles
        val fileValids = files.map {
          _.isValid
        }
        val res = fileValids.length > 0 && fileValids.fold(true) {
          _ && _
        }
        res
      }
      val isEntireSchemaValidAsAnXSD: Boolean = OOLAG.keepGoing(false) {
        this.root.xmlSchemaDocument.schemaFile
          .map { primaryDfdlSchemaFile =>
            primaryDfdlSchemaFile.isValidAsCompleteDFDLSchema
          }
          .getOrElse(true)
      }

      val res = isEachFileIndividuallyValid && isEntireSchemaValidAsAnXSD
      res
    }
  }

  lazy val validationDiagnostics = {
    val files = allSchemaFiles
    val res = files.flatMap {
      _.validationDiagnostics
    }
    res
  }

  lazy val schemas: Seq[Schema] = {
    val schemaPairs = allSchemaDocuments.map { sd => (sd.targetNamespace, sd) }
    //
    // groupBy is deterministic if the hashCode of the key element is deterministic.
    // our NS objects hashCode is same as their underlying string.
    //
    // Alas, being deterministic doesn't mean it is in an order we expect.
    // but at least it is deterministic.
    val schemaGroups = schemaPairs.groupBy {
      _._1
    } // group by the namespace identifier
    val schemas = schemaGroups.map {
      case (ns, pairs) => {
        val sds = pairs.map { case (ns, s) => s }
        val sch = Schema(ns, sds.toSeq, this)
        sch
      }
    }
    schemas.toSeq
  }

  lazy val globalSimpleTypeDefs: Seq[GlobalSimpleTypeDef] =
    schemas.flatMap(_.globalSimpleTypeDefs)

  /**
   * For checking uniqueness of global definitions in their namespaces
   */

  private type UC = (NS, String, Symbol, GlobalComponent)

  private lazy val allTopLevels: Seq[UC] = LV(Symbol("allTopLevels")) {
    val res = schemas.flatMap { schema =>
      {
        val ns = schema.namespace
        val geds = schema.globalElementDecls.map { g =>
          {
            (ns, g.name, Symbol("Element"), g)
          }
        }
        val stds = schema.globalSimpleTypeDefs.map { g =>
          {
            (ns, g.name, Symbol("SimpleType"), g)
          }
        }
        val ctds = schema.globalComplexTypeDefs.map { g =>
          {
            (ns, g.name, Symbol("ComplexType"), g)
          }
        }
        val gds = schema.globalGroupDefs.map { g =>
          {
            (ns, g.name, Symbol("Group"), g)
          }
        }
        val dfs = schema.defineFormats.map { g =>
          {
            (ns, g.name, Symbol("DefineFormat"), g)
          }
        }
        val dess = schema.defineEscapeSchemes.map { g =>
          {
            (ns, g.name, Symbol("DefineEscapeScheme"), g)
          }
        }
        val dvs = schema.defineVariables.map { g =>
          {
            (ns, g.name, Symbol("DefineVariable"), g)
          }
        }
        val all = geds ++ stds ++ ctds ++ gds ++ dfs ++ dess ++ dvs
        all
      }
    }
    res.asInstanceOf[Seq[UC]]
  }.value

  private lazy val groupedTopLevels = LV(Symbol("groupedTopLevels")) {
    val grouped = allTopLevels.groupBy {
      case (ns, name, kind, obj) => {
        (kind, ns, name)
      }
    }
    val grouped2 = grouped.map {
      case (idFields @ (kind, ns, name), seq) => {
        val locations = seq.map {
          case (_, _, _, obj: LookupLocation) => obj
          case _ => Assert.invariantFailed("not (ns, name, kind, lookuplocation)")
        }
        if (locations.length > 1) {
          SDEButContinue(
            "multiple definitions for %s %s%s.\n%s",
            kind.name.toString,
            (if (ns eq NoNamespace) "" else "{" + ns.toString + "}"),
            name,
            locations
              .map {
                _.locationDescription
              }
              .mkString("\n")
          )
        }
        (idFields, locations)
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
  private def findRootElement(name: String): GlobalElementDecl = {
    val candidates = schemas.flatMap {
      _.getGlobalElementDecl(name)
    }
    schemaDefinitionUnless(
      candidates.nonEmpty,
      "No root element found for %s in any available namespace",
      name
    )
    schemaDefinitionUnless(
      candidates.length <= 1,
      "Root element %s is ambiguous. Candidates are %s.",
      name,
      candidates.map { gef =>
        {
          val tns = gef.schemaDocument.targetNamespace
          Assert.invariant(!tns.isUnspecified)
          gef.name + " " + tns.explainForMsg
        }
      }
    )
    Assert.invariant(candidates.length == 1)
    val ge = candidates.head
    ge
  }

  /**
   * Given a RootSpec, get the global element it specifies. Error if ambiguous
   * or not found.
   */
  private def getGlobalElement(rootSpec: RootSpec) = {
    rootSpec match {
      case RootSpec(Some(rootNamespace), rootElementName) => {
        val qn = RefQName(None, rootElementName, rootNamespace)
        val optGE = getGlobalElementDecl(qn)
        val ge = optGE.getOrElse {
          schemaDefinitionError("No global element found for %s", rootSpec)
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
   * You can define the root by passing the root specification to the Compiler.compileX method.
   *
   * Or, you can leave the root unspecified, and this method will determine it from the
   * first element declaration of the first schema file.
   */
  lazy val root: Root = LV(Symbol("root")) {
    val re: GlobalElementDecl =
      optPFRootSpec match {
        case Some(rs) =>
          getGlobalElement(rs)
        case None => {
          // if the root element and rootNamespace aren't provided at all, then
          // the first element of the first schema document is the root
          val sDocs = this.allSchemaDocuments
          assuming(sDocs.nonEmpty)
          val firstSchemaDocument = sDocs.head
          val gdecl = firstSchemaDocument.globalElementDecls
          val firstElement = {
            schemaDefinitionUnless(
              gdecl.nonEmpty,
              "No global elements in: " + firstSchemaDocument.uriString
            )
            gdecl.head
          }
          firstElement
        }
      }
    re.asRoot
  }.value

  /**
   * Retrieve schema by namespace
   *
   * If the schema has no namespace, then use NoNamespace
   */
  def getSchema(namespace: NS): Option[Schema] = {
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }

  /**
   * XML Schema global objects.
   * Given a namespace and name, try to retrieve the named object
   *
   * These all return factories for the objects, not the objects themselves.
   */
  def getGlobalElementDecl(refQName: RefQName): Option[GlobalElementDecl] = {
    getSchema(refQName.namespace).flatMap {
      _.getGlobalElementDecl(refQName.local)
    }
  }

  def getGlobalSimpleTypeDef(refQName: RefQName): Option[GlobalSimpleTypeDef] = {
    getSchema(refQName.namespace).flatMap {
      _.getGlobalSimpleTypeDef(refQName.local)
    }
  }

  def getGlobalComplexTypeDef(refQName: RefQName): Option[GlobalComplexTypeDef] = {
    getSchema(refQName.namespace).flatMap {
      _.getGlobalComplexTypeDef(refQName.local)
    }
  }

  def getGlobalGroupDef(refQName: RefQName): Option[GlobalGroupDef] = {
    getSchema(refQName.namespace).flatMap {
      _.getGlobalGroupDef(refQName.local)
    }
  }

  /**
   * DFDL Schema top-level global objects
   */
  def getDefineFormat(refQName: RefQName): Option[DFDLDefineFormat] = {
    getSchema(refQName.namespace).flatMap {
      _.getDefineFormat(refQName.local)
    }
  }

  def getDefineVariable(refQName: RefQName): Option[DFDLDefineVariable] = {
    val optVar = getSchema(refQName.namespace).flatMap {
      _.getDefineVariable(refQName.local)
    }
    optVar.orElse {
      predefinedVars.find(dfv => {
        dfv.namespace == refQName.namespace && dfv.name == refQName.local
      })
    }
  }

  def getDefineEscapeScheme(refQName: RefQName): Option[DFDLDefineEscapeSchemeFactory] = {
    getSchema(refQName.namespace).flatMap {
      _.getDefineEscapeScheme(refQName.local)
    }
  }

  /**
   * Creates a DFDLDefineVariable object for the predefined variable.
   *
   * @param theName The variable name.
   * @param theType The type of the variable. ex. xs:string
   * @param nsURI   The namespace URI of the variable.
   * @return A Seq[DFDLDefineVariable]
   */
  private def generateDefineVariable(
    theName: String,
    theType: String,
    theDefaultValue: String,
    nsURI: String,
    sdoc: SchemaDocument
  ) = {
    val dfv = DFDLDefineVariable(
      <dfdl:defineVariable name={theName}
                             type={theType}
                             defaultValue={theDefaultValue}
                             external="true"
                             xmlns:xs={XMLUtils.XSD_NAMESPACE.toString}/>,
      sdoc,
      nsURI
    )
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

    val encDFV =
      generateDefineVariable("encoding", "xs:string", "UTF-8", nsURI, schemaDocForGlobalVars)
    val boDFV = generateDefineVariable(
      "byteOrder",
      "xs:string",
      "bigEndian",
      nsURI,
      schemaDocForGlobalVars
    )
    val binDFV = generateDefineVariable(
      "binaryFloatRep",
      "xs:string",
      "ieee",
      nsURI,
      schemaDocForGlobalVars
    )
    val outDFV = generateDefineVariable(
      "outputNewLine",
      "xs:string",
      "%LF;",
      nsURI,
      schemaDocForGlobalVars
    )

    Seq(encDFV, boDFV, binDFV, outDFV)
  }

  lazy val allDefinedVariables = schemas
    .flatMap(_.defineVariables) ++: predefinedVars

  private lazy val checkUnusedProperties = LV(Symbol("hasUnusedProperties")) {
    root.checkUnusedProperties
  }.value

  /**
   * Asking if there are errors drives compilation. First the schema is validated, then
   * the abstract syntax tree is constructed (which could be a source of errors)
   * and finally the AST objects are checked for errors, which recursively
   * demands that all other aspects of compilation occur.
   */
  override lazy val isError: Boolean = {
    if (!isValid) true
    else if (
      // use keepGoing so we can capture errors and
      // return true (indicating there is an error)
      //
      OOLAG.keepGoing(true) {
        !areComponentsConstructed
      }
    ) true
    else {
      // we must check for errors here via the super method, as that iterates over
      // all the objects evaluating them for any errors in their required evaluations.
      // This has to be after everything else that could report an error here (on some
      // other object) has been done.
      //
      // That is to say, if we called super.isError at the top of this method that would
      // be incorrect since isValid and areComponentsConstructed above might cause errors
      // to be recorded or objects created that this call to super.isError would then not
      // take into account.
      val hasErrors = super.isError
      if (!hasErrors) {
        // must be last, after all compilation is done.
        // only check this if there are no errors.
        checkUnusedProperties
      }
      hasErrors
    }
  }

  /**
   * Creation of the DSOM tree of components
   *
   * Assumes schema files exist and are valid.
   *
   * As DSOM objects are constructed, the factory pattern assures
   * that initialize() is called. This demands the basic fields that
   * are needed for diagnostic messages describing error contexts.
   *
   * Then we start from the root set of schema components, and construct
   * all children reachable from them. We "activate" these so that the
   * OOLAG requiredEvaluationsIfActive will execute for these components
   * as a part of SchemaSet.isError (but that execution of those required
   * evaluations happens later).
   *
   * Then we construct the refMap. This is the core data structure allowing
   * shared objects to access all the contexts where they are used.
   *
   * Some LVs like enclosingComponents depend on the refMap, so those cannot
   * be evaluated until this point.
   *
   * Once the refMap is constructed, we are done "construcing" the components.
   *
   * An important characteristic here is that everything we invoke here is
   * downward looking. None of it involves looking upward at enclosing components
   * with the exception of the unique lexical parent.
   *
   * A challenge is that often quite sophisticated analysis is required in order
   * to determine the child components. E.g., one must analyze length-kind to
   * know if a prefix-length quasi-element component is needed. The lengthKind
   * property can be scoped, hence, all the machinery associated with scoped
   * lookup of properties is part of establishing the tree/graph of components.
   * That said, it is a goal to do minimal other work here. Just create all the
   * components.
   *
   * Inability to resolve a QName reference is a fatal error, and no attempt to continue
   * processing accumulating more errors is required in that situation.
   *
   * @return true if the components are all constructed with their interconnections.
   *         false with diagnostics accumulated otherwise.
   */
  private lazy val areComponentsConstructed: Boolean = LV(Symbol("areComponentsConstructed")) {
    allSchemaFiles
    allSchemaDocuments
    schemas
    allSchemaComponents // recursively constructs all the objects.
    allSchemaComponents.foreach { _.setRequiredEvaluationsActive() }
    root.refMap
    true
  }.value

  private lazy val startingGlobalComponents: Seq[SchemaComponent] = {
    root +: {
      // always need the simple type defs so we can ask for their repType elements as part of
      // constructing the complete object graph. By just always obtaining all the simple types
      // defs we insure the quasi-elements used by them are always constructed, and names are
      // resolvable.
      allSchemaDocuments.flatMap { (sd: SchemaDocument) =>
        sd.globalSimpleTypeDefs
      } ++ {
        if (this.checkAllTopLevel)
          allSchemaDocuments.flatMap { (sd: SchemaDocument) =>
            sd.defaultFormat // just demand this since it should be possible to create it.
            sd.globalElementDecls ++
              sd.globalComplexTypeDefs ++
              sd.globalGroupDefs
          }
        else
          Nil
      }
    }
  }

  private lazy val allSchemaComponentsSet = TransitiveClosureSchemaComponents(
    startingGlobalComponents
  )

  lazy val allSchemaComponents: Seq[SchemaComponent] = allSchemaComponentsSet.toIndexedSeq

}

object TransitiveClosureSchemaComponents {
  def apply(ssc: Seq[SchemaComponent]) =
    (new TransitiveClosureSchemaComponents())(ssc)
}

class TransitiveClosureSchemaComponents private () extends TransitiveClosure[SchemaComponent] {

  type SSC = Seq[SchemaComponent]

  override protected def func(sc: SchemaComponent) = {
    val referents: SSC = sc match {
      case asc: AnnotatedSchemaComponent => asc.optReferredToComponent.toSeq
      case _ => Nil
    }
    val children: SSC = sc match {
      case er: AbstractElementRef => Nil // already have referents above.
      case e: ElementDeclMixin =>
        e.typeDef match {
          case std: SimpleTypeDefBase => Seq(std)
          case ctd: ComplexTypeBase => Seq(ctd)
          case pt: PrimitiveType => Nil
        }
      case m: ModelGroup => m.groupMembers
      case st: SimpleTypeDefBase =>
        st.bases ++
          st.optRestriction ++
          st.optUnion
      case r: Restriction => r.optUnion.toSeq
      case u: Union => u.unionMemberTypes
      case c: ComplexTypeBase => Seq(c.modelGroup)
      case gd: GlobalGroupDef => gd.groupMembers
      case stmt: DFDLStatement => Nil
    }
    val statements: SSC = sc match {
      case t: Term => t.statements
      case _ => Nil
    }
    val misc: SSC = sc match {
      case eb: ElementBase => {
        eb.optPrefixLengthElementDecl.toSeq ++
          eb.optRepTypeElementDecl.toSeq
      }
      case _ => Seq()
    }

    referents ++ children ++ statements ++ misc
  }
}
