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
package org.apache.daffodil.runtime2.generators

import scala.collection.JavaConverters._

import org.apache.daffodil.codegen.ast._
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dsom.ElementBase

class ElementParserGenerator(context: ElementBase, contentParserGenerator: ParserGenerator)
  extends ParserGenerator {

  def toPrimitive(primType: NodeInfo.PrimType): TypeDefinition = {
    import NodeInfo.PrimType
    primType match {
      case PrimType.Long => Primitive.LONG
      case PrimType.Int => Primitive.INTEGER
      case _ => context.SDE("Unsupported primitive type: " + primType)
    }
  }

  def toComplexType(e: ElementBase): TypeDefinition = {
    // Stubbed code
    Expressions.typeOf("e2")
  }

  def toParseMethod(e: ElementBase): Method = {
    Expressions.method(Expressions.typeOf("E2_Type"), "parseSelf", Primitive.VOID,
      Nil.asJava, Nil.asJava)
  }

  override def generateCode(cgState: CodeGeneratorState): CodeGeneratorState = {

    val childDecls =
      context.elementChildren.map { child =>
        if (child.optPrimType.isDefined)
          new FieldDeclaration(toPrimitive(child.optPrimType.get), child.name)
        else
          new FieldDeclaration(toComplexType(child), child.name)
      }

    val statements: Seq[Node] = (childDecls zip context.elementChildren).map {
      case (childFD, child) if (child.optPrimType.isDefined) => {
        Expressions.assignment(
          Expressions.field(child.name), // bug: Something wrong here. We get NPE due to this field expression object not having a target
          // but the target should be this object... ???
          Expressions.call(
            Expressions.call(
              cgState.stateExp,
              Expressions.method(
                cgState.pStateType, // object
                "dataInputStream", // method name
                Primitive.VOID, // return type (We don't care about this in this case)
                Nil.asJava, // parameters
                Nil.asJava // exceptions
              )),
            toParseMethod(child),
            toPrimitive(child.optPrimType.get)))
      }
      case (childFD, child) => {
        Assert.invariant(child.optPrimType.isEmpty)
        //
        // generate the allocation of new object, and assignment to variable
        //
        Expressions.assignment(
          Expressions.field(child.name),
          Expressions.new_(toComplexType(child)))
        //
        // generate recursive call to parseSelf
        //
        Expressions.call(
          Expressions.field(child.name),
          Expressions.method(
            toComplexType(child),
            "parseSelf", Primitive.VOID,
            Nil.asJava,
            Nil.asJava))
      }
    }

    val body = {
      val bb = new BlockBuilder
      statements.foreach { bb.add(_) }
      Seq(
        new MethodDefinition("parseSelf", Primitive.VOID, Seq(cgState.stateExp).asJava,
          bb.toBlock()))
    }

    val classDecl = new ClassDeclaration("org.apache.daffodil", context.name + "_Type",
      childDecls.asJava,
      Nil.asJava, // constructors
      body.asJava, // statements
      Nil.asJava // inner classes
    )

    cgState.addClassDecl(classDecl)
    cgState
  }
  // Pseudo-Scala code for this idea is here:
  //
  // Schema that references a global complex type.
  //
  val foo =
    <element name="r">
      <complexType>
        <sequence>
          <element name="e1" type="xs:int"/>
          <element name="e2" type="tns:e2type"/>
        </sequence>
      </complexType>
    </element>
  val e2Type =
    <complexType name="e2Type">
      <sequence>
        <element name="e3" type="xs:int"/>
      </sequence>
    </complexType>
  //    class R_Type() {
  //      var e1: Int = 0 // an element decl just becomes a member definition having that name.
  //      var e2: E2Type = _
  //
  //      def parseSelf(pstate: PState): R_Type = {
  //        //
  //        // This is what we generate for a simple type element
  //        e1 = {
  //          // generated code to get value from nested element e1, which is simple type.
  //          pstate.dataInputStream.getInt32_BigEndian_MSBF_byteAligned()
  //        }
  //        //
  //        // This is what we generate for a complex element
  //        //
  //        e2 = new E2Type()
  //        e2.parseSelf(pstate) // complex type has a parseSelf routine which fills it.
  //
  //        this
  //      }
  //
  //    }
  //
  //    /**
  //     *  Because e2Type is a global complexType definition, this class is global.
  //     *
  //     *  If e2Type had been an anonymous local type definition, this class would have been
  //     *  nested within the above class. (tbd: nesting? maybe not a good idea)
  //     */
  //    class E2Type() {
  //      var e3: Int = 0
  //
  //      def parseSelf(pstate: PState): E2Type = {
  //        e3 = {
  //          // generated code to get value from nested element e1, which is simple type.
  //          pstate.dataInputStream.getInt32_BigEndian_MSBF_byteAligned()
  //        }
  //        this
  //      }
  //    }
  //
  //    ???

}
