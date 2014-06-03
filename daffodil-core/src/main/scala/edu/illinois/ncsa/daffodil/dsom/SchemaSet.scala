package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.collection.JavaConversions.asScalaIterator
import scala.xml.Node
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.grammar.PrimitiveFactoryBase
import edu.illinois.ncsa.daffodil.compiler.RootSpec
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.xml.DFDLSchemaErrorHandler
import edu.illinois.ncsa.daffodil.xml.DFDLSchemaValidationException
import edu.illinois.ncsa.daffodil.xml.DFDLSchemaValidationWarning
import edu.illinois.ncsa.daffodil.xml.DFDLSchemaValidationError
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import java.io.File

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
class SchemaSet(
  externalVariables: Seq[Binding],
  primFactory: PrimitiveFactoryBase,
  schemaFilesArg: Seq[File],
  validateDFDLSchemas: Boolean,
  rootSpec: Option[RootSpec] = None,
  checkAllTopLevelArg: Boolean = false,
  parent: SchemaComponent = null)
  extends SchemaComponent(<schemaSet/>, parent) // a fake schema component
  with SchemaSetIncludesAndImportsMixin {

  override lazy val primitiveFactory = primFactory

  requiredEvaluations({
    isValid
    if (checkAllTopLevel) {
      checkForDuplicateTopLevels()
      this.allTopLevels
    }
    validateSchemaFiles
  })

  override lazy val schemaSet = this
  // These things are needed to satisfy the contract of being a schema component.
  override lazy val enclosingComponent = None
  override lazy val schemaDocument = Assert.usageError("schemaDocument should not be called on SchemaSet")

  /**
   * Let's use the filename for the first schema document, rather than giving no information at all.
   *
   * It would appear that this is only used for informational purposes
   * and as such, doesn't need to be a URL.  Can just be String.
   */
  override lazy val fileName = schemaFilesArg(0).getPath()

  /**
   * We need to use the loader here to validate the DFDL Schema
   */
  private lazy val loader = new DaffodilXMLLoader(DFDLSchemaErrorHandler)

  /**
   * Validates the DFDL Schema files present in the schemaFilesArg.
   * Compiles a list of all errors and warnings before issuing them.
   *
   * Issues SchemaDefinitionWarnings for DFDLSchemaValidationWarnings.
   * Issues SchemaDefinitionErrors for DFDLSchemaValidationErrors.
   */
  private def validateSchemaFiles = {
    // TODO: DFDL-400 remove this flag check once we've fixed all affected tests.
    if (validateDFDLSchemas) {
      val diags: scala.collection.mutable.Queue[(DFDLSchemaValidationException, String)] = scala.collection.mutable.Queue.empty

      schemaFilesArg.foreach(f =>
        try {
          loader.validateDFDLSchema(f)
        } catch {
          case e: DFDLSchemaValidationException => diags.enqueue((e, f.getAbsolutePath()))
        })

      val warn = diags.filter { case (d, _) => d.isInstanceOf[DFDLSchemaValidationWarning] }.map { case (d, f) => d.getMessage() + " File: " + f }
      val errs = diags.filter { case (d, _) => d.isInstanceOf[DFDLSchemaValidationError] }.map { case (d, f) => d.getMessage() + " File: " + f }

      if (warn.length > 0) SDW("DFDL Schema Validation warned due to the following:\n%s", warn.mkString("\n"))
      if (errs.length > 0) SDE("DFDL Schema Validation failed due to the following:\n%s", errs.mkString("\n"))
    }
  }

  lazy val schemaFiles = schemaFilesArg

  lazy val checkAllTopLevel = checkAllTopLevelArg

  override def warn(th: Diagnostic) = oolagWarn(th)
  override def error(th: Diagnostic) = oolagError(th)

  /**
   * This constructor for unit testing only
   */
  def this(primFact: PrimitiveFactoryBase, sch: Node, rootNamespace: String = null, root: String = null, extVars: Seq[Binding] = Seq.empty) =
    this(
      extVars,
      primFact,
      {
        val file = XMLUtils.convertNodeToTempFile(sch)
        val files = List(file)
        files
      },
      false,
      if (root == null) None else {
        if (rootNamespace == null) Some(RootSpec(None, root))
        else Some(RootSpec(Some(NS(rootNamespace)), root))
      },
      false)

  /**
   * Registry used to map from infoset nodes back to the schema components
   * that made them.
   */

  lazy val schemaComponentRegistry = new SchemaComponentRegistry()

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

  lazy val schemas = schemas_.value
  private val schemas_ = LV('schemas) {
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
  }

  /**
   * For checking uniqueness of global definitions in their namespaces
   */

  private type UC = (NS, String, Symbol, SchemaComponent)

  lazy val allTopLevels: Seq[UC] = allTopLevels_.value
  private val allTopLevels_ = LV('allTopLevels) {
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
  }

  lazy val groupedTopLevels = groupedTopLevels_.value
  private val groupedTopLevels_ = LV('groupedTopLevels) {
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
          val obj = onlyObj.head
          val locations = onlyObj.asInstanceOf[Seq[LookupLocation]] // don't like this downcast
          SDEButContinue("multiple definitions for %s  {%s}%s.\n%s", kind.toString, ns, name,
            locations.map { _.locationDescription }.mkString("\n"))
        }
        (idFields, onlyObj)
      }
    }
    val res = grouped2.flatMap { case (_, topLevelThing) => topLevelThing }.toSeq
    res
  }

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
  def findRootElement(name: String) = {
    // log(Info("%s searching for root element with name %s", Misc.getNameFromClass(this), name))
    val candidates = schemas.flatMap { _.getGlobalElementDecl(name) }
    schemaDefinitionUnless(candidates.length != 0, "No root element found for %s in any available namespace", name)
    schemaDefinitionUnless(candidates.length <= 1, "Root element %s is ambiguous. Candidates are %s.",
      candidates.map { gef => gef.name + " in namespace: " + gef.schemaDocument.targetNamespace })
    Assert.invariant(candidates.length == 1)
    val gef = candidates(0)
    val re = gef.forRoot()
    re
  }

  /**
   * Given a RootSpec, get the global element it specifies. Error if ambiguous
   * or not found.
   */
  def getGlobalElement(rootSpec: RootSpec) = {
    rootSpec match {
      case RootSpec(Some(rootNamespaceName), rootElementName) => {
        val geFactory = getGlobalElementDecl(rootNamespaceName, rootElementName)
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
   * Cache of whatever the rootElem decision was
   */
  var rootElemOpt: Option[GlobalElementDecl] = None

  var rootSpecFromPF: Option[RootSpec] = None
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
  def rootElement(rootSpecFromProcessorFactory: Option[RootSpec]): GlobalElementDecl = {
    rootSpecFromPF = rootSpecFromProcessorFactory
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
          val firstSchemaDocument = sDocs(0)
          val gdeclf = firstSchemaDocument.globalElementDecls
          val firstElement = {
            schemaDefinitionUnless(gdeclf.length >= 1, "No global elements in: " + firstSchemaDocument.fileName)
            val rootElement = gdeclf(0).forRoot()
            rootElement
          }
          firstElement
        }
        case _ => Assert.invariantFailed("illegal combination of root element specifications")
      }
    rootElemOpt = Some(re)
    //
    // Show root as either "{...ns...}name" 
    // or if there is no namespace just "name (in no namespace)"
    //
    val (nsSpecifier, comment) =
      if (re.targetNamespace == NoNamespace) ("", " (in no namespace)")
      else ("{" + re.targetNamespace + "}", "")
    // log(Info("Found root element %s%s%s", nsSpecifier, re.name, comment))
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
  def getGlobalElementDecl(namespace: NS, name: String) = {
    // getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
    val s = getSchema(namespace)
    val res = s.flatMap { s =>
      {
        val ged = s.getGlobalElementDecl(name)
        ged
      }
    }
    res
  }
  def getGlobalSimpleTypeDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }

  /**
   * DFDL Schema top-level global objects
   */
  def getDefaultFormat(namespace: NS, name: String) = getSchema(namespace).flatMap { x => Some(x.getDefaultFormat) }
  def getDefineFormat(namespace: NS, name: String) = {
    val s = getSchema(namespace)
    s.flatMap { _.getDefineFormat(name) }
  }
  def getDefineFormats(namespace: NS, context: ThrowsSDE) = getSchema(namespace) match {
    case None => context.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(namespace: NS, name: String) = {
    val res = getSchema(namespace).flatMap { _.getDefineVariable(name) }
    val finalResult = res match {
      case None => {
        val optRes = this.predefinedVars.find(dfv => {
          dfv.namespace == namespace && dfv.name == name
        })
        optRes
      }
      case Some(value) => res
    }
    finalResult
  }
  def getDefineEscapeScheme(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

  def getPrimitiveType(ns: NS, localName: String) = {
    if (ns != XMLUtils.XSD_NAMESPACE) // must check namespace
      None
    else
      PrimType.allPrimitiveTypes.find { _.name == localName }
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
      <dfdl:defineVariable name={ theName } type={ theType } defaultValue={ theDefaultValue } xmlns:xs={ XMLUtils.XSD_NAMESPACE.toString }/>, sdoc) {
      override lazy val expandedNCNameToQName = "{" + nsURI + "}" + theName
      override lazy val namespace = NS(nsURI)
      override lazy val targetNamespace = NS(nsURI)
    }
    dfv
  }

  lazy val schemaDocForGlobalVars = this.schemas(0).schemaDocuments(0)

  // We'll declare these here at the SchemaSet level since they're global.
  lazy val predefinedVars = {
    val extType = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, "string")
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
  def resolveExternalVariableNamespaces(allDefinedVariables: Seq[DFDLDefineVariable]) = {
    var finalExternalVariables: scala.collection.mutable.Queue[Binding] = scala.collection.mutable.Queue.empty

    val extVarsWithoutNS = externalVariables.filterNot(b => b.hasNamespaceSpecified)

    val extVarsWithNS = externalVariables.filter(b => b.hasNamespaceSpecified)

    extVarsWithNS.foreach(b => finalExternalVariables.enqueue(b))

    extVarsWithoutNS.foreach(v => {
      val matchingDVs = allDefinedVariables.filter(dv => dv.name == v.varName)

      matchingDVs.length match {
        case 0 => this.SDE("Could not find the externaly defined variable %s.", v.varName)
        case x: Int if x > 1 =>
          this.SDE("The externally defined variable %s is ambiguous.  " +
            "A namespace is required to resolve the ambiguity.\nFound:\t%s",
            v.varName, matchingDVs.mkString(", "))
        case _ => // This is OK, we have exactly 1 match
      }

      val newNS = matchingDVs.head.namespace
      val newBinding = Binding(v.varName, Some(newNS), v.varValue)
      finalExternalVariables.enqueue(newBinding)
    })
    finalExternalVariables
  }

  lazy val variableMap = {
    val dvs = allSchemaDocuments.flatMap { _.defineVariables }
    val alldvs = dvs.union(predefinedVars)
    val vmap = VariableMap.create(alldvs)

    // At this point we want to try to figure out which, if any, external
    // variables did not have a namespace specified.
    val finalExternalVariables = resolveExternalVariableNamespaces(alldvs)

    val finalVMap =
      ExternalVariablesLoader.loadVariables(finalExternalVariables, this, vmap)

    finalVMap
  }

  private lazy val elements = schemaComponentRegistry.contextMap

  def getIDByName(name: String, ns: org.jdom2.Namespace) = {
    val matches = elements.filter {
      case (uuid, eb) => {
        eb.targetNamespace.toJDOM == ns &&
          eb.name == name
      }
    }
    matches.headOption
  }

  def getSCIDAugmentedInfoset(origInfoset: Node) = {
    val infoset = XMLUtils.elem2Element(origInfoset)

    val rootMatch = getIDByName(infoset.getName(), infoset.getNamespace())

    rootMatch match {
      case None => // Nothing to do here
      case Some((uuid, _)) => {
        infoset.setAttribute("context", uuid.toString(), XMLUtils.INT_NS_OBJECT)
      }
    }

    val it = infoset.getDescendants(new org.jdom2.filter.ElementFilter).asInstanceOf[java.util.Iterator[org.jdom2.Element]]

    for (e <- it) {
      val name = e.getName()
      val ns = e.getNamespace()

      val theMatch = getIDByName(name, ns)
      theMatch match {
        case None => // Nothing to do here
        case Some((uuid, _)) => {
          e.setAttribute("context", uuid.toString(), XMLUtils.INT_NS_OBJECT)
        }
      }

    }

    infoset
  }
}
