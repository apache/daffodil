package edu.illinois.ncsa.daffodil.xml

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.language.reflectiveCalls

/**
 * Please centralize QName handling here.
 *
 * This can be XSD-specific, but should be generic capabilities.
 *
 * There are two concrete classes of QNames for named things: Global and Local
 *
 * There are two concrete classes of referencing QNames, for references to
 * global objects (as in ref="tns:bar" and type="xs:int") and for references
 * which can be to local objects as well as global, such as in DPath step expressions like
 * ../foo:bar/baz.
 *
 * There is a precedent for how xpath expressions work in XSD which is the xpath
 * expressions that are used in the xs:selector and xs:field sub-elements of the
 * xs:key and xs:unique constraints.
 *
 * Based on this, there are two kinds of named things: globally named things, and
 * locally. The only locally named things are local element declarations. Global things
 * include elements, types, groups, etc.
 *
 * These distinctions are necessary because of XSD's elementFormDefault concept.
 * This makes matching against a local name either equivalent to matching against
 * a global name (elementFormDefault="qualified"), or local name matching is
 * namespace independent (elementFormDefault="unqualified"), which is the default
 * behavior.
 *
 * The QName objects are always constructed with a prefix (if present), local
 * name part, and namespace.
 *
 * Consider this case:
 *
 * <xs:element ref="bar" xmlns="someURN".../>
 *
 * In that case, the name "bar" is resolved in the default namespace to be
 * {someURN}bar.
 *
 * However, in the absence of a default namespace binding, the name "bar" will
 * resolve to {No_Namespace}bar, and that will only have a counterpart if
 * there is a schema with no target namespace which defines bar as a global def
 * of some sort.
 *
 * Now consider this case:
 *
 * <xs:schema targetNamespace="someURN">
 *    ...
 * <xs:element name="foo" form="qualified">
 * <xs:complexType>
 * <xs:sequence>
 *      <xs:element name="bar" form="qualfied"..../>
 *
 * Now, in some other place we have a DPath such as
 *
 * <.... xmlns="someURN"
 *  <xs:selector xpath="../foo/bar"/>
 *
 * This does not match because despite the xmlns defining a default namespace,
 * that is not used when considering path steps. Both foo and bar are qualified
 * which means path steps referencing them MUST have prefixes on them.
 * (This is the behavior of Xerces as of 2014-09-29)
 *
 * If there is no default namespace at the point of reference, then
 * this will only match if
 * (1) There is no targetNamespace surrounding the decl of element bar. (Which ought
 *  to draw a warning given that there is no namespace but there is an explicit request
 *  for qualified names.)
 * (2) The schema's element form default is 'unqualified' (which is the
 * default). In this case the local name is the only thing that has to match.
 *
 * This latter case is represented by a StepRef with NoNamespace for the namespace,
 * matching against a LocalDeclQName, which will have NoNamespace by way of
 * the elementFormDefault being unqualified.
 *
 */

/*
 * Use this factory to create the right kinds of QNames.
 */
object QName {

  def resolveRef(qnameString: String, scope: scala.xml.NamespaceBinding) =
    RefQNameFactory.resolveRef(qnameString, scope)

  def resolveStep(qnameString: String, scope: scala.xml.NamespaceBinding) =
    StepQNameFactory.resolveRef(qnameString, scope)

  def createLocal(name: String, targetNamespace: NS, isQualified: Boolean,
    scope: scala.xml.NamespaceBinding) = {
    val ns = if (isQualified) targetNamespace else NoNamespace
    // TODO: where we parse xmlSchemaDocument, a check for
    // xs:schema with no target namespace, but elementFormDefault 'qualified'
    // should emit a warning. It is not ALWAYS incorrect, as a schema
    // designed for inclusion into another schema (via xs:include)
    // can take a position on how its local element names are to
    // be qualified without having a target namespace itself.
    //
    // But,... it is very likely to be an error.
    //
    LocalDeclQName(None, name, ns)
  }

  def createGlobal(name: String, targetNamespace: NS) = {
    GlobalQName(None, name, targetNamespace)
  }
}

trait QNameBase {

  /**
   * The prefix is not generally involved in matching, but they
   * must show up in diagnostic messages. Mistakes by having the wrong prefix
   * or by omitting one, are very common.
   */
  def prefix: Option[String]
  def local: String
  def namespace: NS // No namespace is represented by the NoNamespace object.

  override def toString = toPrettyString

  def toPrettyString: String = {
    (prefix, local, namespace) match {
      case (None, local, ns) if ns.isNoNamespace => local
      case (Some(pre), local, ns) if ns.isNoNamespace => pre + ":" + local
      case (None, local, ns) => "{" + ns + "}" + local
      case (Some(pre), local, ns) => pre + ":" + local + "{xmlns:" + pre + "='" + ns + "'}"
    }
  }

  /**
   * expanded name looks like {...uri...}local.
   * The prefix isn't part of it.
   */
  def toExpandedName: String = {
    if (namespace.isNoNamespace) local
    else "{" + namespace.uri + "}" + local
  }

  def matches[Q <: QNameBase](other: Q): Boolean
}

/**
 * common base trait for named things, both global and local
 */
trait NamedQName
  extends QNameBase {

}

/**
 * QName for a local declaration.
 */
case class LocalDeclQName(prefix: Option[String], local: String, namespace: NS)
  extends NamedQName {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      case StepQName(_, `local`, `namespace`) => {
        // exact match
        //
        // This means either this was unqualified so the namespace
        // arg is NoNamespace, and the local matched that, or...
        // it means this was qualified but the other matched that
        // (which can happen even without a prefix if there is a default
        // namespace i.e., xmlns="...")
        true
      }
      case StepQName(_, `local`, NoNamespace) => {
        //
        // This case matches if the other has no NS qualifier
        // but in this case, this local decl name DOES have
        // some other URI as its namespace, so there is
        // explicitly no match here.
        //
        false
      }
      case StepQName(_, `local`, _) => false // NS didn't match, even if local did we don't care.
      case StepQName(_, notLocal, _) => false
      case _ => Assert.usageError("other must be a StepQName")
    }
  }
}

/**
 * QName for a global declaration or definition (of element, type, group, format, etc.)
 */
case class GlobalQName(prefix: Option[String], local: String, namespace: NS)
  extends NamedQName {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      // StepQNames match against global names in the case of a path
      // step that refers to an element that is defined in its
      // group, via an element reference.
      case StepQName(_, `local`, `namespace`) => true // exact match
      case StepQName(_, _, _) => false
      // RefQNames match against global names in element references,
      // group references, type references (i.e., type="..."), etc.
      case RefQName(_, `local`, `namespace`) => true // exact match
      case RefQName(_, _, _) => false
      case _ => Assert.usageError("other must be a StepQName or RefQName")
    }
  }
}

/**
 * base trait for Qnames that reference other things
 */
trait RefQNameBase extends QNameBase

/**
 * A qname as found in a ref="foo:bar" attribute.
 */
case class RefQName(prefix: Option[String], local: String, namespace: NS)
  extends QNameBase {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      case named: GlobalQName => named.matches(this)
      case _ => Assert.usageError("other must be a GlobalQName")
    }
  }
}

/**
 * A qname as found in a path step as in ../foo:bar/baz/quux
 *
 * Differs from RefQName in that it has to match up to LocalDeclQNames
 * properly.
 */
case class StepQName(prefix: Option[String], local: String, namespace: NS)
  extends QNameBase {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      case named: NamedQName => named.matches(this) // let the named things decide how other things match them.
      case _ => Assert.usageError("other must be a NamedQName")
    }
  }

  /**
   * Finds a match in a list of things that have QNames.
   * Used for finding if a named path step has corresponding element declaration.
   *
   * Handles local or global matches
   *
   */
  def findMatch[T <: { def namedQName: NamedQName }](candidates: Seq[T]): Option[T] = {
    val matched = candidates.find { x =>
      val other = x.namedQName
      val res = matches(other)
      res
    }
    matched
  }

}

trait RefQNameFactoryBase[T] {

  protected def resolveDefaultNamespace(scope: scala.xml.NamespaceBinding): Option[String]

  protected def constructor(prefix: Option[String], local: String, namespace: NS): T

  def resolveRef(qnameString: String, scope: scala.xml.NamespaceBinding): Option[T] = {
    val (prefix, local) = qnameString.split(":").toSeq match {
      case Seq(local) => (None, local)
      case Seq(prefix, local) => (Some(prefix), local)
      case _ => Assert.impossibleCase()
    }
    // note that the prefix, if defined, can never be ""
    val optURI = prefix match {
      case None => resolveDefaultNamespace(scope)
      case Some(pre) => Option(scope.getURI(pre))
    }
    val optNS = (prefix, optURI) match {
      case (None, None) => Some(NoNamespace)
      case (Some(pre), None) => None // Error. Unresolvable Prefix
      case (_, Some(ns)) => Some(NS(ns))
    }
    val res = optNS map { constructor(prefix, local, _) }
    res
  }
}

object RefQNameFactory extends RefQNameFactoryBase[RefQName] {

  override def constructor(prefix: Option[String], local: String, namespace: NS) =
    RefQName(prefix, local, namespace)

  override def resolveDefaultNamespace(scope: scala.xml.NamespaceBinding) =
    Option(scope.getURI(null)) // could be a default namespace
}

object StepQNameFactory extends RefQNameFactoryBase[StepQName] {

  override def constructor(prefix: Option[String], local: String, namespace: NS) =
    StepQName(prefix, local, namespace)

  override def resolveDefaultNamespace(scope: scala.xml.NamespaceBinding) =
    None // don't consider default namespace
}

