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
package org.apache.daffodil.api;


/**
 * Base class used by clients who want to walk the runtime1 metadata information.
 * <p>
 * The runtime1 {@link Metadata} is the aspects of the DFDL schema information that are
 * needed at runtime.
 * <p>
 * Interfacing Daffodil to other data handling systems requires both a metadata bridge
 * be built that takes Daffodil metadata into that system's metadata, and a data bridge
 * that takes Daffodil data to that system's data.
 * <p>
 * Bridging this runtime1 library to other data handling software is most easily done
 * directly from runtime1's metadata and data structures.
 * (The Daffodil Schema Compiler's walkers are an alternative, but are more
 * for interfacing the Daffodil schema compiler data structures to other compiler backends.)
 * <p>
 * This walker/handler works on the pre-compiled binary schema
 * just as well as if the schema was just compiled. This bypasses the need for Daffodil's
 * schema compiler to be involved at all in interfacing to say, Apache Drill or other
 * data fabrics. A pre-compiled DFDL schema is all that is needed.
 */
public abstract class MetadataHandler {
  /**
   * Called for simple type element metadata (for declarations or references)
   * <p>
   * There are no separate start/end methods for simple element declarations.
   *
   * @param m the simple element metadata for the element declaration.
   */
  public abstract void simpleElementMetadata(SimpleElementMetadata m);

  /**
   * Called for complex type element metadata (for declarations or references)
   * <p>
   * Subsequent calls will be for the model group making up the content
   * of the element.
   *
   * @param m the complex element metadata for the element declaration that is starting
   */
  public abstract void startComplexElementMetadata(ComplexElementMetadata m);

  /**
   * Called for complex type element metadata (for declarations or references)
   * <p>
   * This is called after all the calls corresponding to the content of the
   * complex type element.
   *
   * @param m the complex element metadata for the element declaration that is ending
   */
  public abstract void endComplexElementMetadata(ComplexElementMetadata m);

  /**
   * Called for sequence group definitions.
   * <p>
   * Subsequent calls will be for the content of the sequence.
   *
   * @param m the sequence metadata for the sequence definition that is starting
   */
  public abstract void startSequenceMetadata(SequenceMetadata m);

  /**
   * Called for sequence group definitions.
   * <p>
   * This is called after all the calls corresponding to the content
   * of the sequence group.
   *
   * @param m the sequence metadata for the sequence definition that is ending
   */
  public abstract void endSequenceMetadata(SequenceMetadata m);

  /**
   * Called for choice group definitions.
   * <p>
   * Subsequent calls will be for the content of the choice.
   *
   * @param m the choice metadata for the choice definition that is starting
   */
  public abstract void startChoiceMetadata(ChoiceMetadata m);

  /**
   * Called for choice group definitions.
   * <p>
   * This is called after all the calls corresponding to the content
   * of the choice group.
   *
   * @param m the choice metadata for the choice definition that is ending
   */
  public abstract void endChoiceMetadata(ChoiceMetadata m);
}
