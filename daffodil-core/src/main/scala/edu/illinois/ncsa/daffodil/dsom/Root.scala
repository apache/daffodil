package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.grammar.RootGrammarMixin
import edu.illinois.ncsa.daffodil.xml.NamedQName

final class Root(parentArg: SchemaDocument,
  namedQNameArg: NamedQName,
  globalElementDecl: => GlobalElementDecl)
  extends AbstractElementRef(
    <root/>, // % Attribute(None, "ref", Text(refQName.toQNameString), Null), // have to have ref attribute.
    parentArg, 1)
  with RootGrammarMixin {

  override lazy val refQName = namedQNameArg.toRefQName

  override lazy val referencedElement = globalElementDecl

  lazy val rootParseUnparsePolicy = defaultParseUnparsePolicy

  override lazy val isHidden = false

}
