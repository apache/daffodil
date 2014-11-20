package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.ElementDeclMixin
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl

trait ElementDeclGrammarMixin { self: ElementBase with ElementDeclMixin =>

  override lazy val inputValueCalcOption = findPropertyOption("inputValueCalc")

}

trait GlobalElementDeclGrammarMixin
  extends LocalElementGrammarMixin // can be repeating if not root
  { self: GlobalElementDecl =>

  lazy val documentElement = Prod("documentElement", this, scalarDefaultable)

  lazy val document = Prod("document", this, {
    UnicodeByteOrderMark(this) ~ documentElement
  })

}

