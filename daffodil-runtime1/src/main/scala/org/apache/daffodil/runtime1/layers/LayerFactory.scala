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

package org.apache.daffodil.runtime1.layers

import java.lang.reflect.Constructor
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
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

object LayerFactory {

  /**
   * This prefix is appended onto the method names of getters that return values from the layer.
   * These getters are called and the corresponding variables are set to have those values. 
   */
  private def varResultPrefix = "getLayerVariableResult_"
  private def varParamSetter = "setLayerVariableParameters"

  /** cache that maps spiName of layer to the LayerVarsRuntime */
  private lazy val alreadyCheckedLayers =
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
   * @param lrd
   * @param protoLayer the layer instance allocated by the SPI loader (zero-arg constructed)
   * @return
   */
  def computeLayerVarsRuntime(lrd: LayerRuntimeData, protoLayer: Layer): LayerVarsRuntime = {
    val optLayerVarsRuntime = alreadyCheckedLayers.get(protoLayer.name())
    optLayerVarsRuntime.getOrElse {
      // we know there is a default zero arg constructor
      // find another constructor and check that there is an argument
      // corresponding to each of the layer variables.
      val c = protoLayer.getClass
      val constructor = c.getConstructors.find { c => c.getParameterCount == 0 }.getOrElse {
        Assert.invariantFailed("SPI-loaded class without default constructor? Not possible.")
      }
      val allMethods = c.getMethods
      val optParamSetter = allMethods.find { _.getName == varParamSetter }
      val allVarResultGetters =
        ListSet(allMethods.filter { m =>
          val nom = m.getName
          nom.startsWith(varResultPrefix)
        }.toSeq: _*)

      if (lrd.vmap.isEmpty && optParamSetter.isEmpty && allVarResultGetters.isEmpty) {
        // there are no vars, we're done
        new LayerVarsRuntime(constructor, None, Nil, Nil)
      } else {
        val allLayerVRDs = ListSet(lrd.vmap.toSeq.map(_._2): _*)
        // there is either a params setter, a result getter, or both.
        val paramVRDs: Seq[VariableRuntimeData] =
          optParamSetter.toSeq.flatMap { paramSetter =>
            // there is a param setter with args that are supposed to correspond to bound vars
            val params = paramSetter.getParameters.toSeq

            val paramTypes = paramSetter.getParameterTypes.toSeq
            val pVRDs: Seq[VariableRuntimeData] = params.map { p =>
              lrd.vmap.getOrElse(
                p.getName,
                lrd.context.SDE(
                  s"No layer DFDL variable named '$p.getName' was found in namespace ${lrd.namespace}.",
                ),
              )
            }.toSeq

            // Now we deal with the result getters and the corresponding vars
            //

            //
            // verify only Int and String are allowed simple types for now.
            // The rest of the code beyond here doesn't care what the type is, but we don't want to
            // create a bunch of test code for layer things that aren't needed.
            //
            allLayerVRDs.foreach { vrd =>
              lrd.context.schemaDefinitionUnless(
                vrd.primType == PrimType.String ||
                  vrd.primType == PrimType.Int ||
                  vrd.primType == PrimType.UnsignedShort ||
                  vrd.primType == PrimType.UnsignedInt,
                s"Layer variable ${vrd.globalQName.toQNameString} of type ${vrd.primType.dfdlType} must" +
                  s" be of type xs:int, xs:unsignedInt, xs:unsignedShort, or xs:string.",
              )
            }
            // at this point the number of vars and number of setter args match

            val typePairs = (pVRDs.zip(paramTypes)).toSeq
            typePairs.foreach { case (vrd, pt) =>
              val vrdClass = PrimType.toJavaType(vrd.primType.dfdlType)
              lrd.context.schemaDefinitionUnless(
                compatibleTypes(vrdClass, pt),
                s"""Layer setter argument ${vrd.globalQName.local} and the corresponding
                   |Layer DFDL variable have differing types: ${pt.getName}
                   | and ${vrdClass.getName} respectively.""".stripMargin,
              )
            }
            if (pVRDs.length != params.length) {
              val msg =
                s"""Layer class $c does not have a parameter setter method named $varParamSetter.
                   | It should have this method with arguments for
                   | each of the layer's parameter variables.""".stripMargin
              lrd.context.SDE(msg)
            }
            pVRDs.toSeq
          }
        val nParams = paramVRDs.length
        val nVars = lrd.vmap.size
        val returnVRDs = allLayerVRDs -- paramVRDs // set subtraction

        // each returnVRD needs to have a corresponding getter method
        val returnVRDNames = returnVRDs.map(_.globalQName.local)
        val resultGettersNames = allVarResultGetters.map(_.getName.replace(varResultPrefix, ""))
        val nResultGetters = resultGettersNames.size

        def javaParamSetterArgs =
          paramVRDs
            .map {
              case vrd => {
                s"type: ${PrimType.toJavaTypeString(vrd.primType.dfdlType)} name: ${vrd.globalQName.local}"
              }
            }
            .mkString(", ")

        def badConstructorMsg: String = {
          s"""Layer class $c does not have a setter with arguments for each of the layer's variables.
             | It should have a setter named $varParamSetter with these arguments in any order, such as
             | ($javaParamSetterArgs)""".stripMargin
        }

        lrd.context.schemaDefinitionUnless(nParams + nResultGetters == nVars, badConstructorMsg)

        val returnVRDsWithoutGetters = returnVRDNames -- resultGettersNames
        val resultGettersWithoutVRDs = resultGettersNames -- returnVRDNames
        lrd.context.schemaDefinitionUnless(
          returnVRDsWithoutGetters.isEmpty,
          s"""The layer variables ${returnVRDsWithoutGetters.mkString(
              ",",
            )} have no corresponding getters.""",
        )
        lrd.context.schemaDefinitionUnless(
          resultGettersWithoutVRDs.isEmpty, {
            val getterFullNames = returnVRDsWithoutGetters.map { vname =>
              this.varResultPrefix + vname
            }
            s"""The getters ${getterFullNames.mkString(
                ",",
              )} have no corresponding layer variables."""
          },
        )
        // at this point we know each variable that was not a parameter of the constructor
        // has a getter with matching name.
        val resultVarPairs = resultGettersNames.map { rgn: String =>
          val getter: Method =
            allVarResultGetters
              .find { g: Method => g.getName == varResultPrefix + rgn }
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
               | and ${gt.getName} respectively.""".stripMargin,
          )
          lrd.context.schemaDefinitionUnless(
            getter.getParameterCount == 0,
            s"""Layer return variable getter ${getter.getName} must have no arguments.""",
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

}

/**
 * Factory for a layer
 *
 * This is the serialized object which is saved as part of a processor.
 * It constructs the layer at runtime when newInstance() is called.
 *
 * This allows the layer instance itself to be stateful and not serializable.
 */
class LayerFactory(val layerRuntimeData: LayerRuntimeData) extends Serializable {
  import LayerFactory._

  /**
   * Call at runtime to create a layer object. This layer object can be stateful
   * and non-thread safe.
   *
   * Called by the LayeredSequenceParser or LayeredSequenceUnparser to allocate the
   * layer, and the result is used to carry out the layering mechanism.
   *
   * @param lri the layer runtime info which includes both static and runtime
   *           state-based information for the parse or unparse
   * @return the Layer properly initialized/constructed for this layer
   */
  def newInstance(lri: LayerRuntimeImpl): LayerDriver = {
    val optCache = alreadyCheckedLayers.get(lri.layerRuntimeData.spiName)
    val layerVarsRuntime: LayerVarsRuntime = optCache.getOrElse {
      val spiLayerInstance =
        LayerRegistry.find(lri.layerRuntimeData.spiName, lri.layerRuntimeData.context)
      // Since layer implementations are dynamically loaded, we must re-verify that
      // the layer implementation matches the DFDL schema's layer variable definitions.
      // This prevents using a layer class file that doesn't match the schema, at
      // least as far as the number and type of the DFDL variables it consumes and writes goes.
      // However, we want to do this exactly once, not every time this method is called,
      // So there is a cache of instances that have already been through these checks,
      // at runtime.
      // We compute this data structure once only at the time the SPI Loads the layer
      // class.
      // In addition, this process of verifying the DFDL variables used by the layer
      // pre-computes some data structures that facilitate fast run-time processing

      val lvr = computeLayerVarsRuntime(lri.layerRuntimeData, spiLayerInstance)
      alreadyCheckedLayers.put(lri.layerRuntimeData.spiName, lvr)
      lvr
    }

    // The LayerRegistry is not going to reload the class every time we ask for it.
    // Rather, it is going to load it once, constructing it with the default zero-arg
    // constructor that they all must have.

    val newInstance = layerVarsRuntime.constructAndCallParamSetterWithParameterVars(lri)

    //
    // The layer driver object is responsible for calling getters for any return values and
    // assigning those values to the return variables. That will happen perhaps quite asynchronously
    // to this call and the invocation of the layer, especially for unparsing where the final
    // return values may be delayed by the need to evaluate suspended computations.
    //
    new LayerDriver(layerRuntimeData, newInstance, layerVarsRuntime)
  }
}

/**
 * Enables fast construction of the layer instance passing all parameter vars values
 * as arguments to the parameter setter method.
 *
 * Also contains the data structures which facilitate fast invocation of the getters for
 * any return result values, and assignment of those values to the layer result variables.
 *
 * This object is NOT serializable. It is transient. It is created and discarded
 * when a DFDL schema that uses a layer is compiled. It is re-created at runtime when
 * such a schema is used for parse/unparse.
 */
class LayerVarsRuntime(
  constructor: Constructor[_],
  optParamSetter: Option[Method],
  paramVRDs: Seq[VariableRuntimeData],
  resultVarPairs: Seq[(VariableRuntimeData, Method)],
) {

  /**
   * Assembles the parameter variables in the proper order, and gets all their values, then
   * calls the corresponding constructor to create the Layer instance, providing all the
   * parameter variables as args to the constructor.
   * @param lr the layer runtime object which provides access to the state, including the
   *           runtime variable instances to be read and set.
   * @return
   */
  def constructAndCallParamSetterWithParameterVars(lr: LayerRuntimeImpl): Layer = {
    val args = paramVRDs.map { vrd => lr.state.getVariable(vrd, lr.state).value }
    val newObj = constructor.newInstance().asInstanceOf[Layer]
    optParamSetter.foreach(_.invoke(newObj, args: _*))
    newObj
  }

  /**
   * Calls getter methods on the layer for the return value variables, and
   * assigns the gotten result values to the return value variables.
   * @param layer the layer from which we are getting the result values
   * @param lr the runtime environment for the layer
   *
   * When parsing this is called in the unwinding when we remove the layer.
   *
   * When unparsing it's trickier. We call this from the close of the data output stream
   * that underlies the layer. That is, from the close() method of
   * `runtime1.layers.JavaIOOutputStream`.
   */
  def callGettersToPopulateResultVars(layer: Layer, lr: LayerRuntimeImpl): Unit = {
    resultVarPairs.foreach { case (vrd, method) =>
      val value = method.invoke(layer)
      val dv = DataValue.unsafeFromAnyRef(value)
      lr.state.setVariable(vrd, dv, lr.state)
    }
  }
}
