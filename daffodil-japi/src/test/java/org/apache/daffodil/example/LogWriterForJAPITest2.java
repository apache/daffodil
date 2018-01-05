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

package org.apache.daffodil.example;

import org.apache.daffodil.japi.logger.*;

import java.util.ArrayList;

public class LogWriterForJAPITest2 extends LogWriter {
	ArrayList<String> errors = new ArrayList<String>();
	ArrayList<String> warnings = new ArrayList<String>();
	ArrayList<String> infos = new ArrayList<String>();
	ArrayList<String> others = new ArrayList<String>();

	public void write(LogLevel level, String logID, String msg) {
		switch (level) {
		case Error:
			errors.add(msg);
			break;
		case Warning:
			warnings.add(msg);
			break;
		case Info:
			infos.add(msg);
			break;
		default:
			others.add(msg);
		}
	}

	public String prefix(LogLevel level, String logID) {
		String prefix;
		switch (level) {
		case Error:
			prefix = "[error] ";
			break;
		case Warning:
			prefix = "[warning] ";
			break;
		case Info:
			prefix = "[info] ";
			break;
		case Compile:
			prefix = "[compile] ";
			break;
		case Debug:
			prefix = "[debug] ";
			break;
		case DelimDebug:
			prefix = "[delimdebug] ";
			break;
		case OOLAGDebug:
			prefix = "[oolagdebug] ";
			break;
		default:
			prefix = "[unknown] ";
		}
		return "[JAPI LOG] " + prefix;
	}

	public String suffix(LogLevel level, String ogID) {
		return " [END]";
	}

}
