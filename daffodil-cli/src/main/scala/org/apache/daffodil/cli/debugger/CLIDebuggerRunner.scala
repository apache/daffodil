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

package org.apache.daffodil.cli.debugger

import java.io.File
import java.io.InputStream
import java.io.PrintStream
import scala.io.Source
import scala.jdk.CollectionConverters._

import org.apache.daffodil.runtime1.debugger._

import org.jline.reader.Candidate
import org.jline.reader.Completer
import org.jline.reader.EndOfFileException
import org.jline.reader.LineReader
import org.jline.reader.LineReaderBuilder
import org.jline.reader.ParsedLine
import org.jline.reader.UserInterruptException
import org.jline.terminal.TerminalBuilder
import org.jline.terminal.impl.DumbTerminal

class CLIDebuggerRunner(cmdsIter: Iterator[String], in: InputStream, out: PrintStream)
  extends InteractiveDebuggerRunner {
  private val prompt = "(debug) "

  def this(in: InputStream = System.in, out: PrintStream = System.out) = {
    this(Iterator.empty, in, out)
  }

  def this(file: File, in: InputStream, out: PrintStream) = {
    this(Source.fromFile(file).getLines(), in, out)
  }

  def this(seq: Seq[String], in: InputStream, out: PrintStream) = {
    this(seq.iterator, in, out)
  }

  var reader: Option[LineReader] = None

  def init(): Unit = {
    throw new IllegalArgumentException(
      "Must pass in InteractiveDebugger to the init function for CLIDebuggerRunner"
    )
  }

  def init(id: InteractiveDebugger): Unit = {
    // if the in/out parameters aren't the normal stdin/stdout, it's likely
    // either some sort of integration test or something where a DumbTerminal
    // is needed. Otherwise, use the TerminalBuilder which detects OS
    // capabilities and picks the best terminal
    val terminal =
      if ((in ne System.in) || (out ne System.out)) {
        new DumbTerminal(in, out)
      } else {
        TerminalBuilder.builder().build()
      }
    val completer = new CLIDebuggerCompleter(id)
    val r = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .completer(completer)
      .build()
    reader = Some(r)
  }

  def fini(): Unit = {
    reader.map { _.getTerminal.close }
    reader = None
  }

  def getCommand: String = {
    val cmd = if (cmdsIter.hasNext) {
      val line = cmdsIter.next()
      if (line.length > 0) {
        reader.get.getHistory.add(line)
      }
      out.println("%s%s".format(prompt, line))
      line
    } else {
      val line =
        try {
          reader.get.readLine(prompt)
        } catch {
          case _: UserInterruptException => "quit" // Ctrl-C
          case _: EndOfFileException => "quit" // Ctrl-D
        }
      line
    }
    cmd.trim
  }

  def lineOutput(line: String): Unit = {
    out.println("  " + line)
  }
}

class CLIDebuggerCompleter(id: InteractiveDebugger) extends Completer {

  def complete(
    reader: LineReader,
    line: ParsedLine,
    candidates: java.util.List[Candidate]
  ): Unit = {
    // JLine3 completely parses the line, taking care of delmiters/quotes/etc.,
    // and stores it in the ParsedLine, with delimeted fields split up in to the
    // line.words array. The last item in this array is the thing we are
    // trying to autocomplete. Everything preceeding that last word (i.e.
    // line.words.init) are the subcommands, which determines what the possible
    // candidates are of that last word.
    val cmds = line.words.asScala.init

    // iterate over the list of commands to find the last subcommand which is
    // used to determine what possible candidates there are
    val optCmd = cmds.foldLeft(Some(id.DebugCommandBase): Option[id.DebugCommand]) {
      case (optCurCmd, nextCmdName) =>
        optCurCmd match {
          case Some(id.DebugCommandBase.Info) => {
            // We found the info command, even if there are more command names
            // after that, we are going to ignore them and just keep the Info
            // command. This lets use provide and autocomplete multiple info
            // commands at once and complete only the last one, e.g. "info foo
            // bar baz"
            optCurCmd
          }
          case Some(cmd) => {
            // We have the name for the next command, try to find the
            // associated subcommand of the current DebugCommand. If we don't
            // find one, it just means they user typed something that's not a
            // valid command and we have no command to use for completing.
            val nextCmd = cmd.subcommands.find { s =>
              s.name == nextCmdName || s.short == nextCmdName
            }
            nextCmd
          }
          case None => {
            // We previously failed to find a next command, likely because one
            // of the subcommands was misspelled. That means we'll have no idea
            // how to complete the last word, so we'll have no candidates
            None
          }
        }
    }

    optCmd match {
      case Some(cmd) => {
        // We found a command used for autocompleting. All subcommands of the
        // last found command are potential completion candidates. If there
        // are no subcommands, then there are no candidates. Of these
        // canidates, JLine will filter out any candidates that do not match
        // the last word so we don't have to do that. It may also uses these
        // candidates if it looks like there is a type in the word being
        // completed, so it's a bit smarter than just doing .startsWith. We
        // let Jline figure that out.
        cmd.subcommands.foreach { sub => candidates.add(new Candidate(sub.name)) }
      }
      case None => {
        // We found words that weren't actually subcommands, so we don't know
        // how to complete this last thing. We have no candidates
      }
    }
  }
}
