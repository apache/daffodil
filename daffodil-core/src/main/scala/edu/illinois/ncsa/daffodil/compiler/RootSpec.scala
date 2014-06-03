package edu.illinois.ncsa.daffodil.compiler

import edu.illinois.ncsa.daffodil.xml.NS

/**
 * Contains a specification of the root element to be used.
 *
 * The whole RootSpec is generally optional, but if you have one,
 * the namespace part of it is optional as well.
 *
 * When the namespace part is None, it means "you, daffodil, figure out the namespace".
 * Which it will do so long as it is unambiguous.
 */
case class RootSpec(ns: Option[NS], name: String) {
  override def toString() = {
    val nsStr = ns.getOrElse("")
    "{" + nsStr + "}" + name
  }
}