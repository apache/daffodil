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

package org.apache.daffodil.util;

import org.apache.daffodil.lib.util.Misc;
import org.apache.daffodil.lib.util.SimpleNamedLoadableService;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Contains methods for dynamic loading of classes from the class path.
 * <p>
 * It is a constrained simple model where a service is provided by a class having a name() method,
 * and found by users by way of a simple map from names to an instance of the class.
 * <p>
 * The class must have a zero-arg constructor.
 */
public class SimpleNamedServiceLoader {
  private static final Logger logger = Logger.getLogger(SimpleNamedServiceLoader.class.getName());

  /**
   * Load all instances of a particular class from the class path that are
   * declared service providers
   *
   * @param clazz The class to be loaded. E.g., classOf[LayerCompiler]
   * @param <T>   The type to be loaded. Usually this is inferred and isn't explicitly supplied.
   *              It must have a name member returning a string. It must have a default (no-arg) constructor.
   * @return A map from the name (string) to the corresponding instance of the class.
   */
  public static <T extends SimpleNamedLoadableService> java.util.Map<String, T> loadClass(Class<T> clazz) {
    String thingName = Misc.getNameGivenAClassObject(clazz);
    Iterator<T> iter = ServiceLoader.load(clazz).iterator();
    List<T> instanceList = new ArrayList<>();

    while (iter.hasNext()) {
      try {
        instanceList.add(iter.next());
      } catch (ServiceConfigurationError e) {
        logger.warning(String.format("Named service %s failed to load: %s. Enable debug logging for more details", thingName, e.getMessage()));
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        e.printStackTrace(pw);
        pw.close();
        logger.log(Level.FINE, sw.toString());
      }
    }

    Map<String, List<T>> instancesFound = instanceList.stream().collect(Collectors.groupingBy(SimpleNamedLoadableService::name));
    Map<String, T> instanceMap = new HashMap<>();

    for (Map.Entry<String, List<T>> entry : instancesFound.entrySet()) {
      String name = entry.getKey();
      List<T> lc = entry.getValue();
      if (lc.size() == 1) {
        instanceMap.put(name, lc.get(0));
      } else {
        String duplicates = lc.stream()
            .map(Misc::getNameFromClass)
            .collect(Collectors.joining(", "));
        logger.warning(String.format("Duplicate classes for %s found. Ignored: $%s.", thingName, duplicates));
      }
    }

    return instanceMap;
  }
}
