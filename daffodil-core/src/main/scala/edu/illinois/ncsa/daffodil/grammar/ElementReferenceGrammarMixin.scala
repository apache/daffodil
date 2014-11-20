package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.ElementRef

trait ElementReferenceGrammarMixin { self: ElementRef =>
  override lazy val termContentBody = self.referencedElement.termContentBody
}

