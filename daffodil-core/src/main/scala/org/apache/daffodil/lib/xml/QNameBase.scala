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

package org.apache.daffodil.lib.xml

import java.net.URI
import java.net.URISyntaxException
import scala.util.Try

import org.apache.daffodil.lib.equality.TypeEqual
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.UnqualifiedPathStepPolicy

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

  /**
   * Construct a RefQName from a QName string, and a scope.
   */
  def resolveRef(
    qnameString: String,
    scope: scala.xml.NamespaceBinding,
    noPrefixNamespace: NS,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Try[RefQName] =
    RefQNameFactory.resolveRef(qnameString, scope, noPrefixNamespace, unqualifiedPathStepPolicy)

  def parseExtSyntax(extSyntax: String): (Option[String], NS, String) = {
    val res =
      try {
        extSyntax match {
          case QNameRegex.ExtQName(prefix, uriString, local) => {
            val pre = if (prefix eq null) None else Some(prefix)
            val ns = (pre, uriString) match {
              case (Some(pre), "") =>
                throw new ExtendedQNameSyntaxException(Some(extSyntax), None)
              case (Some(pre), null) => UnspecifiedNamespace
              case (Some(pre), s) => NS(s)
              case (None, "") => NoNamespace
              case (None, null) => UnspecifiedNamespace
              case (None, s) => NS(s)
            }
            (pre, ns, local)
          }
          case _ => throw new ExtendedQNameSyntaxException(Some(extSyntax), None)
        }
      } catch {
        case ex: URISyntaxException =>
          throw new ExtendedQNameSyntaxException(Some(extSyntax), Some(ex))
        case ia: IllegalArgumentException => {
          val ex = ia.getCause()
          if (ex ne null)
            throw new ExtendedQNameSyntaxException(Some(extSyntax), Some(ex))
          else
            throw ia
        }
      }
    res
  }

  /**
   * Specialized getQName function for handling
   * manually specified variables via the CLI.
   *
   * Variables will be of the format:
   *
   * 1. {nsURI}varName=value
   * 2. {}varName=value       // explicitly means NoNamespace
   * 3. varName=value         // unspecified namespace, i.e., might default
   */
  def refQNameFromExtendedSyntax(extSyntax: String): Try[RefQName] = Try {
    val (pre, ns, local) = parseExtSyntax(extSyntax)
    val res = RefQName(pre, local, ns)
    res
  }

  def resolveStep(
    qnameString: String,
    scope: scala.xml.NamespaceBinding,
    noPrefixNamespace: NS,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Try[StepQName] = {
    // It is not clear what namespace to use when a path step does not have a prefix and there
    // is no default namespace defined. The step could be to a global element in the same
    // schema, in which case noPrefixNamespace is probably the right choice. But if the step is
    // to a local element declaration and elementFormDefault=unqualifed then no-namespace should
    // be used. Or if a path step references an element in a completely different schema, then
    // it depends on the namespaces declared in that schema. So we can't really say for sure
    // what namespace to use.
    //
    // So we ignore the passed in noPrefixNamespace parameter and instead pass in NoNamespace to
    // resolveRef. Resolving steps will use the unqualifiedPathStepPolicy, which will cause it
    // to use either the default namespace or NoNamespace for unprefixed steps.
    //
    // This will likely change as part of DAFFODIL-2917.
    StepQNameFactory.resolveRef(qnameString, scope, NoNamespace, unqualifiedPathStepPolicy)
  }

  def createLocal(
    name: String,
    targetNamespace: NS,
    isQualified: Boolean,
    scope: scala.xml.NamespaceBinding
  ) = {
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
    LocalDeclQName(optPrefix(scope, ns), name, ns)
  }

  private def optPrefix(scope: scala.xml.NamespaceBinding, targetNamespace: NS) = {
    val res =
      if (scope eq null) None
      else if (targetNamespace == NoNamespace) None
      else if (targetNamespace == UnspecifiedNamespace) None
      else {
        val prefix = scope.getPrefix(targetNamespace)
        if (prefix eq null) None
        else Some(prefix)
      }
    res
  }

  def createGlobal(name: String, targetNamespace: NS, scope: scala.xml.NamespaceBinding) = {
    GlobalQName(optPrefix(scope, targetNamespace), name, targetNamespace)
  }

  /**
   * Printing utility for QNames. Allows you control over the individual components.
   * @param prefix if defined, will be included in printed representation
   * @param local the local name (NCName)
   * @param namespace an NS object
   * @return
   */
  def toPrettyString(prefix: Option[String], local: String, namespace: NS): String = {
    (prefix, local, namespace) match {
      case (Some(pre), local, NoNamespace) =>
        Assert.invariantFailed("QName has prefix, but NoNamespace")
      case (Some(pre), local, UnspecifiedNamespace) => pre + ":" + local
      case (None, local, NoNamespace) => "{}" + local
      case (None, local, UnspecifiedNamespace) => local
      case (None, local, ns) => "{" + ns + "}" + local
      case (Some(pre), local, ns) => pre + ":" + local
    }
  }
}

protected sealed abstract class QNameSyntaxExceptionBase(
  kind: String,
  offendingSyntax: Option[String],
  cause: Option[Throwable]
) extends Exception(offendingSyntax.getOrElse(null), cause.getOrElse(null)) {

  override def getMessage = {
    val intro = "Invalid syntax for " + kind + " "
    val details = (offendingSyntax, cause) match {
      case (Some(syntax), Some(cause)) => "'%s'. Caused by: '%s'".format(syntax, cause)
      case (None, Some(cause)) => "'%s'".format(cause.getMessage())
      case (Some(syntax), None) => "'%s'.".format(syntax)
      case _ => Assert.usageError("supply either offendingSyntax, or cause or both")
    }
    intro + details
  }
}

class ExtendedQNameSyntaxException(offendingSyntax: Option[String], cause: Option[Throwable])
  extends QNameSyntaxExceptionBase("extended QName", offendingSyntax, cause)

class QNameSyntaxException(offendingSyntax: Option[String], cause: Option[Throwable])
  extends QNameSyntaxExceptionBase("QName", offendingSyntax, cause)

class QNameUndefinedPrefixException(pre: String)
  extends Exception("Undefined QName prefix '%s'".format(pre))

trait QNameBase extends Serializable {

  /**
   * The prefix is not generally involved in matching, but they
   * must show up in diagnostic messages. Mistakes by having the wrong prefix
   * or by omitting one, are very common.
   */
  def prefix: Option[String]
  def local: String
  def namespace: NS // No namespace is represented by the NoNamespace object.

  override def toString = toPrettyString

  /**
   * For purposes of hashCode and equals, we disregard the prefix
   */
  override lazy val hashCode = namespace.hashCode + local.hashCode

  override def equals(other: Any) = {
    val res = other match {
      case qn: QNameBase => (local =:= qn.local && namespace =:= qn.namespace)
      case _ => false
    }
    res
  }

  /**
   * Provides name with namespace information. Uses prefix if that is
   * appropriate, otherwise puts out the namespace in braces. Empty braces are
   * the no-namespace indicator.
   *
   * Incorrectly defined names are not tolerated.
   */
  def toPrettyString: String = QName.toPrettyString(prefix, local, namespace)

  /**
   * displays all components that are available.
   */
  def toExtendedSyntax: String = {
    (prefix, local, namespace) match {
      case (Some(pre), local, NoNamespace) =>
        Assert.invariantFailed("QName has prefix, but NoNamespace")
      case (Some(pre), local, UnspecifiedNamespace) =>
        Assert.invariantFailed("QName has prefix, but unspecified namespace")

      case (None, local, NoNamespace) => "{}" + local
      case (None, local, UnspecifiedNamespace) => local
      case (None, local, ns) => "{" + ns + "}" + local
      //
      // This is a hack to avoid printing out tns prefixes just becasue a
      // schema author has chosen to use the tns prefix for the same uri as
      // the target namespace. Really we only want to print out a prefix
      // if it's a meaningful prefix that will distinguish something.
      //
      case (Some("tns"), local, ns) => "{" + ns + "}" + local // ignore the "tns" prefix.
      case (Some(pre), local, ns) => pre + ":{" + ns + "}" + local
    }
  }

  /**
   * Never displays the namespace. Never complains about inconsistencies.
   * Provides back what the schema author ought to think of as the name
   * of the thing.
   */
  def diagnosticDebugName: String = {
    (prefix, local, namespace) match {
      case (Some(pre), local, NoNamespace) =>
        pre + ":" + local // generally this is an error. Shouldn't have a prefix.
      case (Some(pre), local, UnspecifiedNamespace) => pre + ":" + local
      case (None, local, NoNamespace) => local
      case (None, local, UnspecifiedNamespace) => local
      case (None, local, ns) => local
      //
      // See comment above about tns hack
      //
      case (Some("tns"), local, ns) => local
      case (Some(pre), local, ns) => pre + ":" + local
    }
  }

  /**
   * Creates a string suitable for use in an XML attribute as in 'dfdl:terminator="..."' or
   * 'xsi:nil="true"'
   *
   * This does not know about the scope, so it cannot decide that a namespace is the default namespace
   * so that just a local name is ok. That distinction actually requires you to know what kind of
   * name it is. RefQName or StepQName, so you can know whether it needs unqualifiedPathStepPolicy or not.
   */
  def toAttributeNameString: String = {
    (prefix, local, namespace) match {
      case (None, local, NoNamespace) => local
      case (None, local, ns) =>
        Assert.invariantFailed("QName has namespace, but no prefix defined.")
      case _ => toPrettyString
    }
  }

  /**
   * Just turns into a prefix (optionally) then the local name e.g, foo:bar
   * or if there is no prefix, just bar.
   */
  def toQNameString: String = {
    if (prefix.isDefined) prefix.get + ":" + local else local
  }

  def matches[Q <: QNameBase](other: Q): Boolean
}

/**
 * common base trait for named things, both global and local
 */
sealed abstract class NamedQName(prefix: Option[String], local: String, namespace: NS)
  extends QNameBase {

  if (prefix.isDefined) {
    Assert.usage(!namespace.isNoNamespace)
    Assert.usage(!namespace.isUnspecified)
  }

  lazy val toRefQName = RefQName(prefix, local, namespace)
}

/**
 * QName for a local declaration.
 */
final case class LocalDeclQName(prefix: Option[String], local: String, namespace: NS)
  extends NamedQName(prefix, local, namespace) {

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
      case StepQName(_, `local`, _) =>
        false // NS didn't match, even if local did we don't care.
      case StepQName(_, notLocal, _) => false
      case _ => Assert.usageError("other must be a StepQName")
    }
  }
}

/**
 * QName for a global declaration or definition (of element, type, group, format, etc.)
 */
final case class GlobalQName(prefix: Option[String], local: String, namespace: NS)
  extends NamedQName(prefix, local, namespace) {

  /**
   * This gets called a lot, even in runtime assertions, so let's make this
   * as fast as possible.
   * @param other
   * @tparam Q
   * @return
   */
  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other.local == this.local && other.namespace == this.namespace
  }
}

/**
 * base trait for Qnames that reference other things
 */
protected sealed trait RefQNameBase extends QNameBase

/**
 * A qname as found in a ref="foo:bar" attribute, or a type="foo:barType" attribute.
 * Or a variable reference e.g., \$foo:barVar in an expression.
 *
 * These are references to globally defined things.
 */
final case class RefQName(prefix: Option[String], local: String, namespace: NS)
  extends RefQNameBase {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      case named: GlobalQName => named.matches(this)
      case _ => Assert.usageError("other must be a GlobalQName")
    }
  }

  lazy val toStepQName = StepQName(prefix, local, namespace)
  lazy val toGlobalQName = GlobalQName(prefix, local, namespace)
}

/**
 * A qname as found in a path step as in ../foo:bar/baz/quux
 *
 * Differs from RefQName in that it has to match up to LocalDeclQNames
 * properly.
 */
final case class StepQName(prefix: Option[String], local: String, namespace: NS)
  extends RefQNameBase {

  override def matches[Q <: QNameBase](other: Q): Boolean = {
    other match {
      case named: NamedQName =>
        named.matches(this) // let the named things decide how other things match them.
      case _ => Assert.usageError("other must be a NamedQName")
    }
  }
}

protected trait RefQNameFactoryBase[T] {

  protected def resolveDefaultNamespace(
    scope: scala.xml.NamespaceBinding,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Option[NS]

  protected def constructor(prefix: Option[String], local: String, namespace: NS): T

  /**
   * Resolve a reference to a QName. Note that the reference may or may not be prefixed. This
   * function supports references to global declarations or to local declarations when resolving
   * steps in DFDL path expressions. Note that step and non-step references are handled exactly
   * the same except for differences in how the default namespace is used for unprefixed
   * references. This difference is implemented by the resolveDefaultNamespace function.
   */
  final def resolveRef(
    qnameString: String,
    scope: scala.xml.NamespaceBinding,
    noPrefixNamespace: NS,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Try[T] = Try {
    qnameString match {
      case QNameRegex.QName(prefix, local) => {
        // note that the prefix, if defined, can never be ""
        val pre = Option(prefix)
        val preNS = pre.map { p =>
          Option(scope.getURI(p)).map(NS(_)).getOrElse {
            throw new QNameUndefinedPrefixException(p)
          }
        }
        // if we have a prefix, use its associated namespace. If we don't, we use the default
        // namespace for this type of resolution (i.e. ref vs step). If that does not exist,
        // then we use the noPrefixNamespace, which is based on targetNamespace and
        // include/import
        val ns = preNS
          .orElse(resolveDefaultNamespace(scope, unqualifiedPathStepPolicy))
          .getOrElse(noPrefixNamespace)
        val res = constructor(pre, local, ns)
        res
      }
      case _ => throw new QNameSyntaxException(Some(qnameString), None)
    }
  }
}

object RefQNameFactory extends RefQNameFactoryBase[RefQName] {

  override def constructor(prefix: Option[String], local: String, namespace: NS) =
    RefQName(prefix, local, namespace)

  /**
   * For normal QNames, (i.e. non-expression steps), we just return the default namespace if one
   * exists
   */
  override def resolveDefaultNamespace(
    scope: scala.xml.NamespaceBinding,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Option[NS] =
    Option(scope.getURI(null)).map(NS(_))

}

object StepQNameFactory extends RefQNameFactoryBase[StepQName] {

  override def constructor(prefix: Option[String], local: String, namespace: NS) =
    StepQName(prefix, local, namespace)

  /**
   * When resolving step QNames without a prefix, we use the unqualifiedPathStepPolicy to
   * determine whether or not to use the default namespace if one exists.
   *
   * This is what needs Tunables and propagates into Expression
   */
  override def resolveDefaultNamespace(
    scope: scala.xml.NamespaceBinding,
    unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  ): Option[NS] = {
    unqualifiedPathStepPolicy match {
      case UnqualifiedPathStepPolicy.NoNamespace =>
        Some(NoNamespace)
      case UnqualifiedPathStepPolicy.DefaultNamespace =>
        Option(scope.getURI(null)).map(NS(_))
      case UnqualifiedPathStepPolicy.PreferDefaultNamespace =>
        Option(scope.getURI(null)).map(NS(_))
    }
  }
}

object QNameRegex {
  private val xC0_D6 = ("""\x{C0}-\x{D6}""")
  private val xD8_F6 = """\x{D8}-\x{F6}"""
  private val xF8_2FF = """\x{F8}-\x{2FF}"""
  // private val x370_37D = """\x{370}-\x{37D}"""
  private val x37F_1FFF = """\x{37F}-\x{1FFF}"""
  private val x200C_200D = """\x{200c}|\x{200d}"""
  private val x2070_218F = """\x{2070}-\x{218F}"""
  private val x2C00_2FEF = """\x{2C00}-\x{2FEF}"""
  private val x3001_D7FF = """\x{3001}-\x{D7FF}"""
  private val xF900_FDCF = """\x{F900}-\x{FDCF}"""
  private val xFDF0_FFFD = """\x{FDF0}-\x{FFFD}"""
  private val x10000_EFFFF = """\x{10000}-\x{EFFFF}"""
  private val range0_9 = """0-9"""
  //  private val xB7 = """\xB7"""
  //  private val x0300_036F = """\x{0300}-\x{036F}"""
  //  private val x203F_2040 = """\x{203F}-\x{2040}"""

  private val ncNameStartChar = "A-Z_a-z" + xC0_D6 + xD8_F6 + xF8_2FF +
    // x370_37D + // TODO: why is this one is left out? Add comments please.
    x37F_1FFF + x200C_200D +
    x2070_218F + x2C00_2FEF + x3001_D7FF + xF900_FDCF + xFDF0_FFFD + x10000_EFFFF
  private val ncNameChar =
    ncNameStartChar + "\\-" + "\\." + range0_9 // + "|" + xB7 + "|" + x0300_036F + "|" + x203F_2040
  // TODO: why are the above left out? Add comments please.
  private val NCNameRegexString = "([" + ncNameStartChar + "](?:[" + ncNameChar + "])*)"
  private val QNameRegexString = "(?:" + NCNameRegexString + "\\:)?" + NCNameRegexString
  lazy val NCName = NCNameRegexString.r
  lazy val QName = QNameRegexString.r

  //
  // extended syntax now supports a prefix and a namespace
  // E.g., ex:{http://example.com}foo
  //
  private val URIPartRegexString = """(?:\{(.*)\})"""
  private val PrefixPartRegexString = """(?:""" + NCNameRegexString + """\:)"""
  private val ExtQNameRegexString =
    PrefixPartRegexString + "?" + URIPartRegexString + "?" + NCNameRegexString
  lazy val ExtQName = ExtQNameRegexString.r

  def isURISyntax(s: String): Boolean = {
    try {
      new URI(s)
      true
    } catch {
      case _: URISyntaxException => false
    }
  }
}
