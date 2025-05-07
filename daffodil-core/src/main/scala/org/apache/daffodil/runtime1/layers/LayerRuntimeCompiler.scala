/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.apache.daffodil.runtime1.layers

import java.lang.reflect.Method
import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import scala.collection.immutable.ListSet
import scala.collection.mutable

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.SimpleNamedServiceLoader
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

class LayerRuntimeCompiler {

  /**
   * This prefix is appended onto the method names of getters that return values from the layer.
   * These getters are called and the corresponding variables are set to have those values. 
   */
  private def varResultPrefix = "getLayerVariableResult_"
  private def varParamSetter = "setLayerVariableParameters"

  /** cache that maps spiName of layer to the LayerVarsRuntime */
  private[layers] lazy val alreadyCheckedLayers =
    new mutable.LinkedHashMap[String, LayerVarsRuntime]()

  /**
   * Computes the things that can be computed once-only to make calling the constructor
   * and passing the parameter variables faster at runtime. This also does all the
   * checking that the constructor has an argument for each variable of properly matching type, and
   * has a getter for each result variable, again returning the proper type.
   *
   * This is called at schema compile time to ensure the layer code is properly defined and matches
   * the layer variable definitions.
   *
   * It is called again at runtime after the Layer class is loaded by the SPI
   * to ensure that the loaded layer class constructor signature at least matches the layer
   * variables defined in the schema.
   *
   * @param lrd the layer static schema compiler-determined information
   * @param protoLayer the layer instance allocated by the SPI loader (zero-arg constructed)
   * @return the runtime structure used to construct, optionally provide DFDL variable values
   *         as parameters, initialize, optinally get results which are stored-back to DFDL variables.
   */
  private def computeLayerVarsRuntime(
    lrd: LayerRuntimeData,
    protoLayer: Layer
  ): LayerVarsRuntime = {
    val optLayerVarsRuntime = alreadyCheckedLayers.get(protoLayer.name())
    optLayerVarsRuntime.getOrElse {
      val c = protoLayer.getClass
      val constructor = c.getConstructor() // cannot fail because SPI loader would have failed
      val allMethods = c.getMethods
      val optParamSetter = allMethods.find { _.getName == varParamSetter }
      val allVarResultGetters =
        ListSet(allMethods.filter { m =>
          val nom = m.getName
          nom.startsWith(varResultPrefix)
        }.toSeq: _*)

      if (lrd.qNameToVRD.isEmpty && optParamSetter.isEmpty && allVarResultGetters.isEmpty) {
        // there are no vars, so no setter, and no getter(s). We're done
        new LayerVarsRuntime(constructor, None, Nil, Nil)
      } else {
        val allLayerVRDs = ListSet(lrd.qNameToVRD.toSeq.map(_._2): _*)
        // there is either a params setter, a result getter, or both.
        val paramVRDs: Seq[VariableRuntimeData] =
          optParamSetter.toSeq.flatMap { paramSetter =>
            // there is a param setter with args that are supposed to correspond to bound vars
            // it is allowed for it to have zero args. Then it's purpose is just initialization, but it
            // must still get called.
            val params = paramSetter.getParameters.toSeq
            val paramTypes = paramSetter.getParameterTypes.toSeq
            val pVRDs: Seq[VariableRuntimeData] = (params.zip(paramTypes)).map { case (p, pt) =>
              val vrd = lrd.qNameToVRD.getOrElse(
                p.getName,
                lrd.context.SDE(
                  s"No layer DFDL variable named '${p.getName}' was found in namespace ${lrd.namespace}."
                )
              )
              val vrdClass = PrimType.toJavaType(vrd.primType.dfdlType)
              lrd.context.schemaDefinitionUnless(
                compatibleTypes(vrdClass, pt),
                s"""Layer setter argument ${vrd.globalQName.local} and the corresponding
                   |Layer DFDL variable have differing types: ${pt.getName}
                   | and ${vrdClass.getName} respectively.""".stripMargin
              )
              vrd
            }

            // Now we deal with the result getters and the corresponding vars
            //
            pVRDs
          }
        val nParams = paramVRDs.length
        val nVars = lrd.qNameToVRD.size
        val returnVRDs = allLayerVRDs -- paramVRDs // set subtraction

        // each returnVRD needs to have a corresponding getter method
        val returnVRDNames = returnVRDs.map(_.globalQName.local)
        val resultGettersNames = allVarResultGetters.map(_.getName.replace(varResultPrefix, ""))
        val nResultGetters = resultGettersNames.size

        val javaParamSetterArgs =
          allLayerVRDs
            .map { vrd =>
              s"type: ${PrimType.toJavaTypeString(vrd.primType.dfdlType)} name: ${vrd.globalQName.local}"
            }
            .mkString(", ")

        lrd.context.schemaDefinitionUnless(
          nParams + nResultGetters == nVars,
          s"""Layer class $c does not have a setter with arguments for each of the layer's variables.
             | It should have a setter named $varParamSetter with an argument for each layer parameter, in any order, such as
             | ($javaParamSetterArgs), and a getter for remaining layer variables, named with a specific
             |  name prefix like: ' $varResultPrefix '.""".stripMargin
        )

        val returnVRDsWithoutGetters = returnVRDNames -- resultGettersNames
        val resultGettersWithoutVRDs = resultGettersNames -- returnVRDNames
        lrd.context.schemaDefinitionUnless(
          returnVRDsWithoutGetters.isEmpty,
          s"""The layer variables ${returnVRDsWithoutGetters.mkString(
              ","
            )} have no corresponding getters."""
        )
        lrd.context.schemaDefinitionUnless(
          resultGettersWithoutVRDs.isEmpty, {
            val getterFullNames = returnVRDsWithoutGetters.map { vname =>
              this.varResultPrefix + vname
            }
            s"""The getters ${getterFullNames.mkString(
                ","
              )} have no corresponding layer variables."""
          }
        )
        // at this point we know each variable that was not a parameter of the setter
        // has a getter with matching name.
        val resultVarPairs = resultGettersNames.map { (rgn: String) =>
          val getter: Method =
            allVarResultGetters
              .find { (g: Method) => g.getName == varResultPrefix + rgn }
              .getOrElse {
                Assert.invariantFailed("no getter for getter name.")
              }
          val vrd = returnVRDs.find { vrd => vrd.globalQName.local == rgn }.getOrElse {
            Assert.invariantFailed("no vrd for getter name.")
          }
          (vrd, getter)
        }
        resultVarPairs.foreach { case (vrd, getter) =>
          val vrdClass = PrimType.toJavaType(vrd.primType.dfdlType)
          val gt = getter.getReturnType
          lrd.context.schemaDefinitionUnless(
            compatibleTypes(vrdClass, gt),
            s"""Layer return variable ${vrd.globalQName.local} and the corresponding
               |Layer getter have differing types: ${vrdClass.getName}
               | and ${gt.getName} respectively.""".stripMargin
          )
          lrd.context.schemaDefinitionUnless(
            getter.getParameterCount == 0,
            s"""Layer return variable getter ${getter.getName} must have no arguments."""
          )
        }
        val lrv =
          new LayerVarsRuntime(constructor, optParamSetter, paramVRDs, resultVarPairs.toSeq)
        alreadyCheckedLayers.put(lrd.spiName, lrv)
        lrv
      }
    }
  }

  /**
   * Tolerates both boxed and unboxed flavors of the primitive types
   * @param vrdClass - layer variable's type as a class object
   * @param pt - setter arg type, or getter result type from reflection
   * @return true if the types are compatible meaning a DFDL variable can supply the parameter, or receive the result.
   */
  private def compatibleTypes(vrdClass: Class[_], pt: Class[_]): Boolean = {
    vrdClass == pt || compatibleBoxedPrimType(vrdClass, pt)
  }

  private val LongPrimClass = classOf[Long]
  private val IntPrimClass = classOf[Int]
  private val ShortPrimClass = classOf[Short]
  private val BytePrimClass = classOf[Byte]
  private val FloatPrimClass = classOf[Float]
  private val DoublePrimClass = classOf[Double]
  private val BooleanPrimClass = classOf[Boolean]

  private def compatibleBoxedPrimType(vrdClass: Class[_], pt: Class[_]): Boolean = {
    pt match {
      case LongPrimClass => vrdClass == classOf[JLong]
      case IntPrimClass => vrdClass == classOf[JInt]
      case ShortPrimClass => vrdClass == classOf[JShort]
      case BytePrimClass => vrdClass == classOf[JByte]
      case FloatPrimClass => vrdClass == classOf[JFloat]
      case DoublePrimClass => vrdClass == classOf[JDouble]
      case BooleanPrimClass => vrdClass == classOf[JBoolean]
      case _ => false
    }
  }

  private lazy val layerMap: Map[String, Layer] =
    SimpleNamedServiceLoader.loadClass[Layer](classOf[Layer])

  private def findSPILayer(spiName: String, lrd: LayerRuntimeData): Layer = {
    val optLayer: Option[Layer] = layerMap.get(spiName)
    optLayer.getOrElse {
      val choices = layerMap.keySet.mkString(", ")
      lrd.context.SDE(
        "The dfdlx:layer '%s' was not found. Available choices are: %s",
        spiName,
        choices
      )
    }
  }

  private def compile(layerRuntimeData: LayerRuntimeData): LayerVarsRuntime = {
    val optLayerVarsRuntime = alreadyCheckedLayers.get(layerRuntimeData.spiName)
    optLayerVarsRuntime.getOrElse {
      val layer = findSPILayer(layerRuntimeData.spiName, layerRuntimeData)
      val res = computeLayerVarsRuntime(layerRuntimeData, layer)
      alreadyCheckedLayers.put(layerRuntimeData.spiName, res)
      res
    }
  }

  /**
   * Called twice. Once during schema compilation time (for when the compiled schema
   * is immediately used), once when a compiled schema is reloaded.
   *
   * The two calls are necessary because the resulting structure, which is needed at runtime,
   * is not serializable, so can't be saved from schema compilation time as part of the
   * runtime data.
   *
   * Any errors that occur are schema definition errors.
   *
   * Note that user-written layer code is not run at this time, but method signatures
   * of setters/getters are checked against the DFDL variables defined in
   * the layer's target namespace.
   */
  def compileAll(lrds: Seq[LayerRuntimeData]): Unit = {
    lrds.foreach { compile(_) }
  }

  /**
   * Requires that the argument is for a layer that has already been runtime compiled.
   * @param lrd
   * @return the LayerVarsRuntime object for the layer.
   */
  def getLayerVarsRuntime(lrd: LayerRuntimeData): LayerVarsRuntime = {
    alreadyCheckedLayers.get(lrd.spiName).getOrElse {
      Assert.invariantFailed("layer was not already compiled.")
    }
  }
}
