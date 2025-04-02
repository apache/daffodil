/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.externalvars

import scala.xml.Node

import org.apache.daffodil.lib.iapi.UnqualifiedPathStepPolicy
import org.apache.daffodil.lib.xml._

/**
 * Represents a variable binding expressed in a config file as a bind element, or specified
 * as options to the CLI (pairs of extendedSyntaxName + value).
 *
 * scope is only relevant for a config file, which can have QNames with prefixes that need to
 * be resolved locally relative to that XML document.
 *
 * For a binding coming from a config file, we require the QName to be "correct".
 *
 * For bindings coming from the CLI options, we may construct bindings and RefQName objects here
 * that do not in fact connect to anything. It is the caller/user of these objects in the runtime
 * system or schema compiler that decides if these names actually refer to any variables.
 */
class Binding(
  val varQName: RefQName,
  val varValue: String,
  scope: scala.xml.NamespaceBinding = null
) {

  override def toString() = {
    "<binding name='" + varQName + "'>" + varValue + "</binding>"
  }

  def hasNamespaceSpecified: Boolean = !varQName.namespace.isUnspecified

  override def hashCode = varQName.hashCode

  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.varQName == that.varQName
      case _ => false
    }
  }

  def globalQName =
    if (scope ne null)
      QName.createGlobal(varQName.local, varQName.namespace, scope)
    else
      GlobalQName(varQName.prefix, varQName.local, varQName.namespace)
}

case class BindingException(message: String)
  extends Exception("Exception when processing external variable binding: %s".format(message))

/**
 * This object is for cases when external variable bindings
 * are passed in via the Command Line Interface.
 */
object Binding {

  /**
   * Make a binding for extended syntax (typically comes from command line options to the CLI).
   *
   * extSyntax is {uri}ncName, or {}ncName, or ncName
   */
  def apply(extSyntax: String, value: String): Binding = try {
    val tryRefQName = QName.refQNameFromExtendedSyntax(extSyntax)
    new Binding(tryRefQName.get, value, null)
  } catch {
    case e: Throwable => throw BindingException(e.getMessage)
  }

  /**
   * Parses one XML bind element like <daf:bind name="foo:bar">baz</daf:bind>
   */
  def apply(node: Node): Binding = {
    val name = (node \ "@name").head.text
    val value = node.text
    val scope = node.scope
    //
    // Variable names are always resolved like names which may refer to a default namespace
    // if one is defined.
    //
    val rqn =
      RefQNameFactory.resolveRef(
        name,
        scope,
        NoNamespace,
        UnqualifiedPathStepPolicy.DefaultNamespace
      )
    new Binding(rqn.get, value)
  }

  /**
   * Supports API construction of external variable bindings.
   *
   * These are allowed to leave out the namespace information if the variable reference
   * would be unambiguous across the names of the defined variables of the schema
   * just based on the local name part alone.
   */
  def apply(local: String, optNamespace: Option[NS], value: String): Binding = try {
    new Binding(RefQName(None, local, optNamespace.getOrElse(UnspecifiedNamespace)), value)
  } catch {
    case e: Throwable => throw BindingException(e.getMessage)
  }

  /**
   * Parses an XML element containing multiple "bind" nodes.
   */
  def getBindings(externalVarBindingsNode: Node): Seq[Binding] = {
    val bindings = externalVarBindingsNode \ "bind"
    try {
      bindings.map(b => Binding(b))
    } catch {
      // FIXME: Should not be catching throwable here.
      // Figure out what might be thrown and catch more specific things.
      // Or consider whether this encapsulation is even needed.
      // Encapsulations that do this really should just pass the cause, not
      // invoke getMessage to create strings.
      // Note that if this encapsulation is removed, unit and other tests that look
      // for binding exception text in negative tests may fail.
      case e: Throwable => throw BindingException(e.getMessage)
    }
  }
}
