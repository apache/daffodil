/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 * Extensively modified - little of the original remains. - Mike Beckerle 2012.
 */

import scala.collection.mutable.Stack
import scala.collection.mutable.Set
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import javax.xml.namespace.QName
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.externalvars.Binding
import util.control.Breaks._
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.xml.GlobalQName

/**
 * Factory for Variable objects
 */
object VariableFactory {
  def create(defv: DFDLDefineVariable,
    globalQName: GlobalQName,
    primType: NodeInfo.PrimType,
    defaultValue: Maybe[String],
    external: Boolean,
    doc: SchemaDocument) = {

    val state =
      if (!defaultValue.isDefined) VariableUndefined
      else VariableDefined

    val compilationTargetType = primType

    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompiler.compile(compilationTargetType, Found(e, defv.dpathCompileInfo))
    }

    val defaultValIsConstant = {
      val isConst = defaultValExpr.map { _.isConstant }.getOrElse(true)
      defv.schemaDefinitionUnless(isConst, "Variable default value %s is not a constant.", defaultValue)
      isConst
    }

    val var_ = Variable(state,
      if (defaultValIsConstant)
        defaultValExpr.map { _.constant.asInstanceOf[AnyRef] }
      else
        None,
      defv.runtimeData,
      defaultValExpr)
    var_
  }
}

object VariableMapFactory {

  def create(dvs: Seq[DFDLDefineVariable]): VariableMap = {
    val pairs = dvs.map { dv => (dv.globalQName, List(List(dv.newVariableInstance))) }
    val hmap = pairs.toMap
    val vmap = new VariableMap(hmap)
    vmap
  }

  def setExternalVariables(currentVMap: VariableMap, bindings: Seq[Binding], referringContext: ThrowsSDE) = {
    var newVMap = currentVMap
    bindings.foreach(b => newVMap = newVMap.setExtVariable(b.globalQName, b.varValue, referringContext))
    newVMap
  }
}
