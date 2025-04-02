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

package org.apache.daffodil.jexample;

import org.apache.daffodil.api.debugger.DebuggerRunner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;


public class DebuggerRunnerForAPITest implements DebuggerRunner {
  ArrayList<String> lines;

  ArrayList<String> commands = new ArrayList<>(Arrays.asList("display info parser",
      "display info bitPosition",
      "display info data",
      "display eval ..",
      "display info diff",
      "trace"));

  Iterator<String> commandsIter;

  public void init() {
    lines = new ArrayList<String>();
    commandsIter = commands.iterator();
  }

  public void fini() {
  }

  public String getCommand() {
    if (commandsIter.hasNext()) {
      return commandsIter.next();
    }

    // If the commandsIter commands are good this should never happen. The
    // only time this would ever get hit is if something caused the
    // debugger to break. But if this does happen, just keep running trace.
    // We should eventually finish parsing.
    return "trace";
  }

  public void lineOutput(String line) {
    lines.add(line + "\n");
  }
}
