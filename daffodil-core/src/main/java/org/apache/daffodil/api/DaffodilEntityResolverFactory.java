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

import org.apache.daffodil.lib.util.SimpleNamedLoadableService;

/**
 * Implement this interface and register it with SPI, by listing the implementation in
 * META-INF/services/org.apache.daffodil.api.DaffodilEntityResolverFactory, to replace the
 * default resolver that Daffodil uses to resolve import/include schemaLocations.
 * <p>
 * Implementations must have a zero-argument constructor. At most one factory may be
 * registered, and factories registered with the same name are ignored.
 * <p>
 * Daffodil calls {@link #make} whenever it needs another resolver and never uses a resolver
 * from more than one thread at a time, so the resolvers need not be thread safe. The factory
 * itself must be thread safe.
 */
public interface DaffodilEntityResolverFactory extends SimpleNamedLoadableService {

  /**
   * Unique name of this factory, used to identify it in diagnostics
   *
   * @return name of the factory
   */
  String name();

  /**
   * @return a new entity resolver
   */
  DaffodilEntityResolver make();
}
