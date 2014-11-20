package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.DFDLStatementMixin
import edu.illinois.ncsa.daffodil.dsom.Term

trait HasStatementsGrammarMixin { self: Term with DFDLStatementMixin =>

  final lazy val statementGrams = statements.map { _.gram }
  // TODO: statements (but specifically not newVariableInstance) can appear on simple type definitions as well as terms.

  lazy val dfdlStatementEvaluations = Prod("dfdlStatementEvaluations", this, statementGrams.length > 0,
    statementGrams.fold(EmptyGram) { _ ~ _ })
}

