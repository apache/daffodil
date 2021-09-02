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

package org.apache.daffodil.processors.parsers

import scala.reflect.macros.blackbox.Context


object PointOfUncertaintyMacros {

  /**
   * For creating a point of uncertainty and ensuring that it is cleaned up
   * appropriately. This allows the use of standard scala-style syntax to
   * create and use points of uncertainties, while avoiding allocating
   * functions. Thus, this allows something like this:
   *
   *   pstate.withPointOfUncertainty { pou =>
   *     // code that implements parsing, and may potentially reset to,
   *     // discard, or resolve this pou variable
   *   }
   *
   * Upon completion of the block, if the new point of uncertainty has not been
   * discarded, reset to, or resovled, this pou will then be discarded,
   * ensuring that Marks are always cleaned up appropriately.
   */
  def withPointOfUncertainty[A, B](c: Context)(pouID: c.Expr[String], context: c.Tree)(func: c.Expr[A => B]) = {

    import c.universe._
    
    val state = TermName(c.freshName("state"))
    val id = TermName(c.freshName("id"))
    val pou = TermName(c.freshName("pou"))
    val ctx = TermName(c.freshName("context"))

    func.tree match {

      // The func tree is something like (param => body), where param is the
      // name the caller wants to name the point of uncertainty mark, and body
      // is the code that uses this point of uncertainty. We transform this
      // code to create a new point of uncertainty, evaluate the body, and then
      // use a finally block to ensure we discard the PoU if it has not been
      // resolved.
      case q"""($param: $_) => $body""" => {

        // The "body" uses the "param" variable name, and scala doesn't like it
        // if we just create a new variable with this name, with a very unclear
        // error message. Instead, we need to create a "fresh" variable name to
        // hold the new point of uncertainty, and then transform the body so
        // that all instances of "param" are replaced with our new fresh name.
        val transformer = new Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Ident(`param`) => Ident(pou)
            case other => super.transform(other)
          }
        }
        val newBody = transformer.transform(c.untypecheck(body))

        q"""{
          // ensure we do not evaluate passed in parameters more than once
          val $state = ${c.prefix}
          val $id = $pouID
          val $ctx = $context

          // create our new point of uncertainty
          val $pou = $state.createPointOfUncertainty($id, $ctx)
          try {
            // evalute the transformed body, which can use this new point of uncertainty
            $newBody
          } finally {
            // if the pou still exists at this point, then simply discard it.
            // This is likely because an exception occurred, or just because we
            // do not require that parsers discard PoUs, with the understanding
            // that it will always happen here
            if (!$state.isPointOfUncertaintyResolved($pou)) {
              $state.discardPointOfUncertainty($pou)
            }
          }
        }"""
      }
    }

  }
}
