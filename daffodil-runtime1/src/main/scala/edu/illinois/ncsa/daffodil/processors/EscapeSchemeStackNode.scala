package edu.illinois.ncsa.daffodil.processors

abstract class EscapeSchemeStackNodeBase

class EscapeSchemeStackNode(val scheme: EscapeScheme) 
extends EscapeSchemeStackNodeBase {
  override def toString() = "<EscapeSchemeStackNode>" + scheme.toString() + "</EscapeSchemeStackNode>"
}

case class EscapeKindNoneStackNode()
  extends EscapeSchemeStackNodeBase {
  
  override def toString() = "<EscapeSchemeStackNode/>"
}