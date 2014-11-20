package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Choice

trait ChoiceGrammarMixin { self: Choice =>

  lazy val groupContent = Prod("choiceContent", this, alternatives.foldRight(mt)(folder))

  def folder(p: Gram, q: Gram): Gram = p | q

  lazy val alternatives = groupMembers.map { _.asTermInChoice }
}

