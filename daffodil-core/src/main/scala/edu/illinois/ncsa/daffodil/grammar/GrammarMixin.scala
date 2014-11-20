package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._

trait GrammarMixin {
  protected val NYI = false // our flag for Not Yet Implemented 

  val prod = Prod.prod
}

