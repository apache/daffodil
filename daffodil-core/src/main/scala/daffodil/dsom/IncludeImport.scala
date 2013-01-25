package daffodil.dsom

import daffodil.util.Misc
import java.net.URL
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.Null
import org.apache.xerces.util.XMLResourceIdentifierImpl
import java.io.IOException
import daffodil.exceptions.Assert
import daffodil.xml.NS
import daffodil.xml.NoNamespace
import daffodil.util.Debug

/**
 * Maps an optional namespace and optional schemaLocation to an Include or Import object.
 *
 * As we include/import schemas, we append to one of these, and before we
 * include/import we check to see if it is already here.
 */
trait IIUtils {
  type IIMap = Map[(NS, URL), SchemaDocument]
}

/**
 * Include/Import = "II" for short
 */
abstract class IIBase(xml: Node, sd: SchemaDocument)
  extends SchemaComponent(xml) {

  lazy val diagnosticChildren = Nil
  lazy val schemaDocument = sd

  lazy val enclosingComponent = Some(sd)
  lazy val schemaLocation = getAttributeOption("schemaLocation")

  /**
   * Both include and import have schemaLocation. For import it is optional.
   * If supplied we resolve it via the classpath.
   */
  lazy val resolvedSchemaLocation: Option[URL] = resolvedSchemaLocation_.value
  private lazy val resolvedSchemaLocation_ = LV('resolvedSchemaLocation) {
    val res = schemaLocation.flatMap { slText =>
      {
        val (optURL, _) = Misc.getResourceOption(slText)
        optURL
      }
    }
    res
  }

  /**
   * Only import has a namespace URI.
   *
   * This will be Some(URL) for reading an imported schema,
   * if we resolved the namespace URI via the XML Catalog.
   */
  def resolvedNamespaceURI: Option[URL]

  /**
   * Holds the location of the schema, whether that is from
   * the XML Catalog (import), or classpath (import or include).
   */
  def resolvedLocation: URL

  /**
   * The included/imported schema document
   *
   * Unfortunately, you can't call super in a lazy val, so
   * we have this trick using an explicitly named 'super' lazy val
   */
  lazy val iiSchemaDocument = superIISchemaDocument // overridable

  final lazy val superIISchemaDocument = superIISchemaDocument_.value
  private lazy val superIISchemaDocument_ = LV('superIISchemaDocument) {
    val sdNode = loader.load(resolvedLocation)
    val newSD = new SchemaDocument(sdNode, sd.schemaSet,
      this match {
        case inc: Include => Some(inc)
        case _: Import => None
        case _ => Assert.invariantFailed("can only be an Include or an Import")
      })
    newSD
  }

  lazy val loader = sd.schemaSet.loader
  lazy val resolver = loader.resolver
  lazy val catFiles = resolver.catalogFiles.mkString(", ")
  lazy val classPathLines = Misc.classPath.filterNot { _.endsWith(".jar") } mkString ("\n")
}

/**
 * enclosingGoalNS is None if this include
 * is being included (by one include hop, or several) into a schema having
 * 'no namespace'
 *
 * enclosingGoalNS is Some(str) if this include
 * is being included (by one include hop, or several) into a schema having
 * a targetNamespace.
 */
class Include(xml: Node, sd: SchemaDocument)
  extends IIBase(xml, sd) {

  lazy val slText = schemaLocation.get // include always has a schemaLocation property

  lazy val prettyName = "include"

  lazy val resolvedNamespaceURI = None // include doesn't have a namespace.

  // include always has a schemaLocation
  lazy val resolvedLocation = resolvedLocation_.value
  private lazy val resolvedLocation_ = LV('resolvedLocation) {
    resolvedSchemaLocation match {
      case Some(rsl) => rsl
      case None => schemaDefinitionError("Included schema not found at location %s.  Searched these locations: \n%s", slText, classPathLines)
    }
  }

}

/**
 * An import statement.
 *
 * The enclosingGoalNamespace argument is Some(noNamespace) for a topLevel schema file
 * that has no targetNamespace attribute.
 *
 * Now consider that we could be an import which is inside an included schema which includes another
 * included, etc. A nest of included schemas the innermost of which then contains an import.
 * We have to verify that the ultimate goal namespace at the start of that chain of includes
 * is different from this imported schema's goalNamespace.
 */
class Import(importNode: Node, sd: SchemaDocument)
  extends IIBase(importNode, sd) {

  val prettyName = "import"

  lazy val importElementNS = getAttributeOption("namespace").map { NS(_) }

  override lazy val iiSchemaDocument = {
    val importedSchemaDoc = superIISchemaDocument
    val importedSchemaDocNS = importedSchemaDoc.targetNamespace
    val tns = targetNamespace

    val checkedNS =
      (importElementNS, importedSchemaDocNS, tns) match {
        case (None, NoNamespace, NoNamespace) =>
          schemaDefinitionError("Namespaces of importing and imported schemas cannot both be no namespace.")
        case (None, NoNamespace, _) => NoNamespace
        case (None, importedSchemaNS, _) =>
          schemaDefinitionError("Import element specifies no namespace, but the imported schema has namespace %s.", importedSchemaNS)
        case (Some(importElementNS), importedSchemaNS, _) if (importElementNS != importedSchemaNS) =>
          schemaDefinitionError("Import element specifies namespace %s but namespace %s of imported schema does not match.", importElementNS, importedSchemaNS)
        case (Some(importElementNS), _, importingSchemaNS) if (importElementNS == importingSchemaNS) =>
          schemaDefinitionError("Importing schema namespace %s and imported schema namespace must be different.", importingSchemaNS)
        case (Some(importElementNS), _, _) => importElementNS
      }
    log(Debug("Imported Schema Namespace: %s", checkedNS))

    importedSchemaDoc
  }

  lazy val resolvedNamespaceURI: Option[URL] = {
    importElementNS match {
      case None => {
        schemaDefinition(schemaLocation != None, "When there is no namespace specified, there must be a schemaLocation specified.")
        None
      }
      case Some(ns) => {
        val uri = resolver.resolveURI(ns.toString)
        val res = uri match {
          case null => None
          case uriString => Some(new URL(uriString))
        }
        res
      }
    }
  }

  /**
   * XML Catalog is tried first, then classpath
   */
  lazy val resolvedLocation = (importElementNS, resolvedNamespaceURI, schemaLocation, resolvedSchemaLocation) match {
    case (None, _, Some(sl), None) =>
      schemaDefinitionError("Unable to import a no-namespace schema from schema location %s. Searched these locations: \n%s", sl, classPathLines)
    case (Some(_), Some(rnURI), _, _) => rnURI // found it in the catalog based on namespace attribute
    case (Some(ns), None, Some(sl), None) =>
      schemaDefinitionError("Unable to import namespace %s from XML catalog(s) %s or schema location %s. Searched these locations: \n%s", ns, catFiles, sl, classPathLines)
    case (_, None, Some(sl), Some(rsl)) => rsl // found it by way of the schemaLocation
    case (Some(ns), None, None, None) => {

      schemaDefinitionError("Unable to import namespace %s using XML Catalog(s) %s", ns, catFiles)
    }
    case _ => Assert.invariantFailed("illegal combination of namespace and schemaLocation")
  }

}
