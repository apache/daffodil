package edu.illinois.ncsa.daffodil.example;

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import edu.illinois.ncsa.daffodil.japi.*;
import edu.illinois.ncsa.daffodil.japi.logger.*;

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
