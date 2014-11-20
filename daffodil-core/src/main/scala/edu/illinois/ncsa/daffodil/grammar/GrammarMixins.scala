package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.ComplexTypeBase
import edu.illinois.ncsa.daffodil.dsom.GroupRef

trait GroupRefGrammarMixin { self: GroupRef =>

  def termContentBody = self.group.termContentBody

}

/////////////////////////////////////////////////////////////////
// Types System
/////////////////////////////////////////////////////////////////

trait ComplexTypeBaseGrammarMixin { self: ComplexTypeBase =>
  lazy val mainGrammar = Prod("mainGrammar", self.element,
    ComplexTypeCombinator(this, modelGroup.group.asChildOfComplexType))
}
