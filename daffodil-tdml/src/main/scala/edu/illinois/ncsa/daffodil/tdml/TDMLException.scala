package edu.illinois.ncsa.daffodil.tdml

import edu.illinois.ncsa.daffodil.exceptions.Assert

class TDMLException(msg: String, val causes: Seq[Exception])
  extends Exception(msg, if (causes.length > 0) causes(0) else null) {

  def this(msg: String) = this(msg, Nil)

  def this(cause: Exception) = this(cause.getMessage(), List(cause))

  def this(causes: Seq[Exception]) = this(
    {
      Assert.usage(causes.length > 0)
      causes.map { _.getMessage() }.mkString("\n")
    },
    causes)
}