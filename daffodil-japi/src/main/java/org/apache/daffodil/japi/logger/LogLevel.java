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

package org.apache.daffodil.japi.logger;

/**
 * Logging levels.
 * <p>
 * Error, Warning, and Info are intended for general use. The default is Info.
 * <p>
 * Levels Resolver Compile, Debug, and OOLAGDebug are intended for Daffodil developer
 * use.
 * 
 */
public enum LogLevel {
	Error(10),
	Warning(20),
	Info(30),
	Resolver(35), // For messages about resolving schema locations from namespaces or other.
	Compile(40),
	Debug(50),
	OOLAGDebug(60),
	DelimDebug(70);

	public int id;

	private LogLevel(int id) {
		this.id = id;
	}
}
