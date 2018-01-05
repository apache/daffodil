package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.grammar.RootGrammarMixin
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import scala.xml.UnprefixedAttribute

/**
 * Root is a special kind of ElementRef that has no enclosing group.
 *
 * This is the entity that is compiled by the schema compiler.
 */
final class Root(defXML: Node, parentArg: SchemaDocument,
  namedQNameArg: NamedQName,
  globalElementDecl: => GlobalElementDecl)
  extends AbstractElementRef(null, parentArg, 1)
  with RootGrammarMixin {

  final override lazy val xml = {
    val elem = XMLUtils.getXSDElement(defXML.scope)
    val res = elem % new UnprefixedAttribute("ref", refQName.toQNameString, scala.xml.Null)
    res
  }

  override lazy val refQName = namedQNameArg.toRefQName

  override lazy val referencedElement = globalElementDecl

  lazy val rootParseUnparsePolicy = defaultParseUnparsePolicy
}
