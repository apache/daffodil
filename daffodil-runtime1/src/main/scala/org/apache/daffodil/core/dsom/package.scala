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

package org.apache.daffodil.core

/**
 * DSOM - DFDL Schema Object Model
 *
 * ==Overview==
 * DSOM is the abstract syntax "tree" of a DFDL schema. It is not actually a tree, it is
 * a graph, as there are back-pointers, and shared objects.
 *
 * A schema is made up of [[SchemaComponent]] objects. A [[SchemaSet]] is a collection of
 * [[Schema]]. A schema is a collection of [[SchemaDocument]] that have a common
 * namespace. The [[SchemaSet]] is the
 * ultimate root of all the objects in a compilation unit. The [[Term]] class is the
 * base for everything that can have a representation in the data stream. .
 *
 * Many [[SchemaComponent]] carry DFDL annotations; hence, [[AnnotatedSchemaComponent]]
 * is a key base trait.
 *
 * ==UML Class Diagram==
 *
 * See the
 * [[https://cwiki.apache.org/confluence/display/DAFFODIL/DFDL+Schema+Object+Model+%28DSOM%29+with+UML
 * Daffodil Wiki]]
 * for class diagrams.
 *
 * ==Terminology==
 *
 * Parsing - in this description we are talking about the Daffodil Schema Compiler.
 * So when we refer to "parsing" the XML, we are referring
 * to the recursive descent walk of the DFDL schema, with that schema represented as Scala's
 * `scala.xml.Node` objects. Strictly speaking, the string text in files of the DFDL
 * schema's XML is already parsed into Scala's `scala.xml.Node` objects, but
 * it is the walk through that structure constructing the DSOM tree/graph that we
 * refer to as "parsing" the DFDL schema.
 *
 * ==Principles of Operation==
 *
 * === Constructing the DSOM Graph===
 *
 * The DSOM object graph must be constructed by looking at only the XML without
 * examining any DFDL annotations. The DSOM structure is required in order to
 * implement DFDL's scoping rules for finding annotations including both
 * properties (like dfdl:byteOrder) and statements (like dfdl:assert); hence,
 * one must have the DSOM graph before one can begin
 * accessing DFDL annotations or you end up in cycles/stack-overflows.
 *
 * This requires a careful consideration of class/trait members and methods that
 * are used when constructing the DSOM graph, and those used after the DSOM
 * graph has been created, in order to compile it into the runtime data structures.
 *
 * There are a few exceptions to the above. The dfdl:hiddenGroupRef attribute is one such. It
 * must be local to the [[LocalSequence]] object, and has implications for the parsing
 * of the XML as it implies there should be no children of that xs:sequence.
 * Since it is not scoped, the DSOM graph is not needed in order to access it.
 * Only the local [[LocalSequence]] object and it's [[DFDLSequence]] annotation object.
 * The [[AnnotatedSchemaComponent]] trait provides methods for this local-only
 * property lookup.
 *
 * The DSOM object graph is also needed in order to issue good diagnostic
 * messages from the compiler; hence, Daffodil validates the DFDL schema before
 * parsing it into the DSOM graph. Careful consideration must be given if a
 * SchemaDefinitionError (SDE) is issued while constructing the DSOM graph.
 *
 * If you run into stack-overflows while the DSOM graph is being constructed, the
 * above is a common cause of them, as the SDE diagnostic messaging uses DSOM graph
 * information to construct context information about the error for inclusion
 * in the messages. If the DSOM graph is still being constructed at that time, then
 * this can be circular.
 *
 * ===Using the DSOM Graph===
 *
 * DSOM supports Daffodil schema compilation by way of the
 * `OOLAG` pattern which
 * is an object oriented way of using the
 * [[https://en.wikipedia.org/wiki/Attribute_grammar attribute grammars]] compiler technique.
 *
 * Many attributes (in the attribute grammar sense, nothing to do with XML attributes)
 * are simply Scala lazy val definitions, but some are declared as OOLAG
 * attributes (using `org.apache.daffodil.oolag.OOLAG.OOLAGHost.LV`) which
 * provides for gathering of multiple diagnostic messages (`SchemaDefinitionError`)
 * before abandoning compilation.
 *
 * DFDL schema compilation largely occurs by evaluating lazy val members of DSOM
 * objects. These include the members of the grammar traits
 * (@see [[org.apache.daffodil.core.grammar]] package), which are mixed
 * in to the appropriate DSOM traits/classes.
 *
 * ===FAQ===
 * Q: Why invent this? Why not use XSOM, or the Apache XML Schema library?
 *
 * A:We had trouble with other XML-schema libraries for
 * lack of adequate support for annotations, non-native attributes, and schema
 * documents as first class objects. So DSOM is specific to Daffodil Basically these
 * libraries are more about implementing XML Schema and validation, and not so
 * much about a complex language built on the annotations of the schema. DSOM
 * is really mostly about the annotations.
 *
 */
package object dsom {
  // This package object is just for scaladoc
}
