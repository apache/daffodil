package edu.illinois.ncsa.daffodil.debugger

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

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil
import edu.illinois.ncsa.daffodil.processors.xpath.{ NumberResult, StringResult, BooleanResult, NodeResult, DFDLFunctions }
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.InfosetItem
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.ElementBegin
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.ExecutionMode
import javax.xml.xpath.{ XPathConstants, XPathException }
import jline.console.ConsoleReader
import jline.console.completer.Completer
import jline.console.completer.StringsCompleter
import jline.console.completer.AggregateCompleter
import edu.illinois.ncsa.daffodil.util.Enum

class InteractiveDebugger extends Debugger {

  object DebugState extends Enum {
    sealed abstract trait Type extends EnumValueType
    case object Continue extends Type
    case object Step extends Type
    case object Pause extends Type
    case object Trace extends Type
  }

  case class DebugException(str: String) extends java.lang.Exception {
    override def toString = "error: " + str
  }

  trait Disablable {
    var enabled = true
    def disable = { enabled = false }
    def enable = { enabled = true }
  }

  case class Breakpoint(id: Int, breakpoint: String) extends Disablable {
    var condition: Option[String] = None
  }

  case class Display(id: Int, cmd: Seq[String]) extends Disablable

  object DebuggerConfig {
    /* the max number of lines to tail the infoset, infosetLines <= 0 means print
     * everything */
    var infosetLines: Int = -1

    /* the max number of bytes to display when displaying data */
    var dataLength: Int = 70

    /* the lenght at which to wrap output infoset/data/etc */
    var wrapLength: Int = 80

    /* whether or not to break only on element creation or anytime the element is seen */
    var breakOnlyOnCreation: Boolean = true

    /* whether or not to break on failure */
    var breakOnFailure: Boolean = true

    /* list of breakpoints */
    val breakpoints = collection.mutable.ListBuffer[Breakpoint]()
    var breakpointIndex: Int = 1

    /* list of displays */
    val displays = collection.mutable.ListBuffer[Display]()
    var displayIndex: Int = 1

    /* whether to remove hidden elements when displaying the infoset */
    var removeHidden: Boolean = false
  }

  var debugState: DebugState.Type = DebugState.Pause

  val reader = new ConsoleReader()
  reader.setPrompt("(debug) ")
  reader.addCompleter(DebugCommandBase.completer)

  override def init(parser: Parser) {
    val cmd = readCmd
  }

  override def after(before: PState, after: PState, parser: Parser) {
    if (!isInteresting(parser)) {
      return
    }

    ExecutionMode.usingUnrestrictedMode {
      debugState = debugState match {
        case _ if (after.status != Success && DebuggerConfig.breakOnFailure) => DebugState.Pause
        case DebugState.Continue | DebugState.Trace => {
          findBreakpoint(after, parser) match {
            case Some(bp) => {
              debugPrintln("breakpoint %s: %s   %s".format(bp.id, bp.breakpoint, bp.condition.getOrElse("")))
              DebugState.Pause
            }
            case None => debugState
          }
        }
        case DebugState.Step => DebugState.Pause
        case _ => debugState
      }

      if (debugState == DebugState.Pause || debugState == DebugState.Trace) {
        if (debugState == DebugState.Trace) {
          debugPrintln("----------------------------------------------------------------------")
        }

        DebuggerConfig.displays.filter(_.enabled).foreach { d => runCommand(d.cmd, before, after, parser) }

        if (after.status != Success) {
          debugPrintln("failure:")
          debugPrintln("%s".format(after.diagnostics.head.getMessage), "  ")
        }
      }

      while (debugState == DebugState.Pause) {
        val args = readCmd
        debugState = runCommand(args, before, after, parser)
      }
    }
  }

  private def isInteresting(parser: Parser): Boolean = {
    parser.toString match {
      case "StartSequence" => false
      case "StartChildren" => false
      case "EndSequence" => false
      case "EndChildren" => false
      case _ => {
        if (parser.toString.startsWith("<seq>")) {
          false
        } else if (parser.toString.startsWith("RepExactlyN")) {
          false
        } else if (parser.toString.startsWith("RepAtMostTotalN")) {
          false
        } else {
          true
        }
      }
    }
  }

  private def readCmd(): Seq[String] = {
    val input = reader.readLine()
    val cmd = input match {
      case "" => {
        val history = reader.getHistory
        if (history.size > 0) {
          history.get(history.size - 1).toString
        } else {
          ""
        }
      }
      case null => { // this happens with CTRL-D
        sys.exit(1)
      }
      case _ => input
    }
    cmd.trim.split(" ").filter(_ != "")
  }

  private def evaluateBooleanExpression(expression: String, state: PState, parser: Parser): Boolean = {
    try {
      DFDLFunctions.currentPState = Some(state)
      val compiledExpr = XPathUtil.compileExpression(expression, parser.context.namespaces, parser.context)
      val element = state.infoset.asInstanceOf[InfosetElement]
      val res = element.evalExpression(expression, compiledExpr, state.variableMap, XPathConstants.BOOLEAN)
      res match {
        case BooleanResult(b) => b
        case _ => false
      }
    } catch {
      case e => false
    } finally {
      DFDLFunctions.currentPState = None
    }
  }

  private def findBreakpoint(state: PState, parser: Parser): Option[Breakpoint] = {
    // TODO: detecting if the parser is an ElementBegin parser is a little
    // tricky because of anonymous parsers. So solve this, make sure the parser
    // is an instance of PrimParser, then check if the primitive representing
    // the prim parser is an instance of ElementBegin. This should be changed
    // if we ever get rid of anonymous parsers.
    if (!DebuggerConfig.breakOnlyOnCreation ||
      (parser.isInstanceOf[PrimParser] && parser.asInstanceOf[PrimParser].primitive.isInstanceOf[ElementBegin])) {
      DebuggerConfig.breakpoints
        .filter(_.enabled)
        .filter { bp => bp.breakpoint == parser.context.prettyName || bp.breakpoint == parser.context.path }
        .find { bp =>
          bp.condition match {
            case Some(expression) => evaluateBooleanExpression(expression, state, parser)
            case None => true
          }
        }
    } else {
      None
    }
  }

  private def runCommand(cmd: Seq[String], before: PState, after: PState, parser: Parser): DebugState.Type = {
    try {
      DebugCommandBase(cmd, before, after, parser)
    } catch {
      case e: DebugException => {
        debugPrintln(e)
        DebugState.Pause
      }
    }
  }

  private def debugPrintln(obj: Any = "", prefix: String = "") {
    obj.toString.split("\n").foreach { line =>
      {
        print("  ")
        print(prefix)
        println(line)
      }
    }
  }

  /**********************************/
  /**          Commands            **/
  /**********************************/

  abstract class DebugCommand {
    val name: String
    lazy val short: String = name(0).toString
    val desc: String
    val longDesc: String
    val subcommands: Seq[DebugCommand] = Seq()
    val hidden = false

    def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type

    override def equals(that: Any): Boolean = {
      that match {
        case str: String => (str == name || str == short)
        case _ => super.equals(that)
      }
    }

    def help(args: Seq[String]): Unit = {
      args.length match {
        case 0 => {
          val visibleSubcommands = subcommands.filter(!_.hidden)
          if (name != "") {
            debugPrintln("%s".format(longDesc))
            if (!visibleSubcommands.isEmpty) {
              debugPrintln()
              debugPrintln("Subcommands:")
            }
          }
          val maxLen = visibleSubcommands.foldLeft(0) { (i, c) => i.max(c.name.length) }
          val formatString = "  %-" + maxLen + "s  %s"
          visibleSubcommands.foreach(c => debugPrintln(formatString.format(c.name, c.desc)))
        }
        case _ => {
          val subcmd = args.head
          val subcmdArgs = args.tail
          subcommands.find(_ == subcmd) match {
            case Some(cmd) => cmd.help(subcmdArgs)
            case None => throw new DebugException("unknown command %s".format(subcmd))
          }
        }
      }
    }

    class DebugCommandCompleter(dc: DebugCommand) extends Completer {
      val subcommandsCompleter = new AggregateCompleter(dc.subcommands.sortBy(_.name).map(_.completer): _*)
      def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {
        val cmds = buffer.replaceAll("^\\s+", "").split("(?= )", 2).toList
        val (cmd, args) = cmds match {
          case c :: rest => rest match {
            case a :: Nil => (c, a)
            case Nil => (c, "")
          }
          case Nil => ("", "")
        }

        if (args != "") {
          if (dc == cmd) {
            val trimArgs = args.replaceAll("^\\s+", "")
            val subcandidates = new java.util.ArrayList[CharSequence]
            val newCursor = subcommandsCompleter.complete(trimArgs, cursor, subcandidates)
            val seq = scala.collection.JavaConversions.asScalaBuffer(subcandidates)
            seq.foreach(c => candidates.add(c))
            buffer.lastIndexOf(trimArgs) + newCursor
          } else {
            -1
          }
        } else {
          if (dc.name.startsWith(cmd)) {
            candidates.add(dc.name + " ")
            buffer.lastIndexOf(cmd)
          } else {
            -1
          }
        }
      }
    }

    def completer: Completer = {
      if (subcommands.isEmpty) {
        new StringsCompleter(name)
      } else {
        new DebugCommandCompleter(this)
      }
    }

    // This ensures that there are no naming conflicts (e.g. short form names
    // conflict). This is really just a sanity check, and really only needs to
    // be run whenever names change or new commands are added. Uncomment this
    // and the DebugCommandBase.checkNameConflicts line to do a check when
    // changes are made.
    //
    //def checkNameConflicts() {
    //  val allNames = subcommands.map(_.name) ++ subcommands.filter{ sc => sc.name != sc.short }.map(_.short)
    //  val duplicates = allNames.groupBy{ n => n }.filter{ case(_, l) => l.size > 1 }.keys
    //  if (duplicates.size > 0) {
    //    Assert.invariantFailed("Duplicate debug commands found in '%s' command: ".format(name) + duplicates)
    //  }
    //  subcommands.foreach(_.checkNameConflicts)
    //}
  }

  //DebugCommandBase.checkNameConflicts

  object DebugCommandBase extends DebugCommand {
    val name = ""
    val desc = ""
    val longDesc = ""
    override lazy val short = ""
    override val subcommands = Seq(Break, Clear, Complete, Condition, Continue, Delete, Disable, Display, Enable, Eval, Help, Info, Quit, Set, Step, Trace, UnDisplay)
    def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
      val newState = args.size match {
        case 0 => {
          throw new DebugException("no command specified")
          DebugState.Pause
        }
        case _ => {
          val subcmd = args.head
          val subcmdArgs = args.tail
          subcommands.find(_ == subcmd) match {
            case Some(c) => c(subcmdArgs, prestate, state, parser)
            case None => {
              throw new DebugException("undefined command: %s".format(subcmd))
              DebugState.Pause
            }
          }
        }
      }
      newState
    }

    override def completer: Completer = {
      new DebugCommandCompleter(this) {
        override def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {
          val cmd = buffer.replaceAll("^\\s+", "")
          val subcandidates = new java.util.ArrayList[CharSequence]
          val newCursor = subcommandsCompleter.complete(cmd, cursor, subcandidates)
          val seq = scala.collection.JavaConversions.asScalaBuffer(subcandidates)
          seq.foreach(c => candidates.add(c))
          buffer.lastIndexOf(cmd) + newCursor
        }
      }
    }

    object Break extends DebugCommand {
      val name = "break"
      val desc = "create a breakpoint"
      val longDesc = """|Usage: b[reak] <element_id>
                        |
                        |Create a breakpoint, causing the debugger to stop when the element
                        |with the <element_id> name is created.
                        |
                        |Example: break element.foo""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.length != 1) {
          throw new DebugException("break command requires a single argument")
        } else {
          DebuggerConfig.breakpoints += new Breakpoint(DebuggerConfig.breakpointIndex, args.head)
          DebuggerConfig.breakpointIndex += 1
        }
        DebugState.Pause
      }
    }

    object Clear extends DebugCommand {
      val name = "clear"
      val desc = "clear the screen"
      val longDesc = """|Usage: cl[ear]
                        |
                        |Clear the screen.""".stripMargin
      override lazy val short = "cl"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        print(27: Char)
        print('[')
        print("2J")
        print(27: Char)
        print('[')
        print("1;1H")

        DebugState.Pause
      }
    }

    object Complete extends DebugCommand {
      val name = "complete"
      val desc = "disable all debugger actions and continue"
      val longDesc = """|Usage: comp[lete]
                        |
                        |Continue parsing the input data until parsing is complete. All
                        |breakpoints are ignored.""".stripMargin
      override lazy val short = "comp"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        DebuggerConfig.breakpoints.foreach(_.disable)
        DebuggerConfig.displays.foreach(_.disable)
        DebugState.Continue
      }
    }

    object Condition extends DebugCommand {
      val name = "condition"
      val desc = "set a DFDL expression to stop at breakpoint"
      val longDesc = """|Usage: cond[ition] <breakpoint_id> <dfdl_expression>
                        |
                        |Set a condition on a specified breakpoint. When a breakpoint
                        |is encountered, the debugger only pauses if the DFDL expression
                        |evaluates to true. If the result of the DFDL expressions is not
                        |a boolean value, it is treated as false.
                        |
                        |Example: condition 1 dfdl:occursIndex() = 3""".stripMargin
      override lazy val short = "cond"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.length < 2) {
          throw new DebugException("command requires a breakpoint id and a DFDL expression")
          return DebugState.Pause
        }
        try {
          val id = args.head.toInt
          val expression = args.tail.mkString(" ")
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => b.condition = Some(expression)
            case None => throw new DebugException("breakpoint %i not found".format(id))
          }
        } catch {
          case _ => throw new DebugException("not a valid breakpoint id")
        }
        DebugState.Pause
      }
    }

    object Continue extends DebugCommand {
      val name = "continue"
      val desc = "continue parsing until a breakpoint is found"
      val longDesc = """|Usage: c[ontinue]
                        |
                        |Continue parsing the input data until a breakpoint is encountered. At
                        |which point, pause parsing and display a debugger console to the user.""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        DebugState.Continue
      }
    }

    object Delete extends DebugCommand {
      val name = "delete"
      val desc = "delete a breakpoint"
      val longDesc = """|Usage: d[elete] <breakpoint_id>
                        |
                        |Delete a specific breakpoint.
                        |
                        |Example: delete 1""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.length != 1) {
          throw new DebugException("delete command requires a single argument")
          return DebugState.Pause
        }
        try {
          val id = args.head.toInt
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => DebuggerConfig.breakpoints -= b
            case None => throw new DebugException("breakpoint %i not found".format(id))
          }
        } catch {
          case _ => throw new DebugException("not a valid breakpoint id")
        }
        DebugState.Pause
      }
    }

    object Disable extends DebugCommand {
      val name = "disable"
      val desc = "disable breakpoints and displays"
      val longDesc = """|Usage: dis[able] <type> <id>
                        |
                        |Disable a breakpoint or display. Valid values for <type> are "breakpoint"
                        |and "display".
                        |
                        |Example: disable breakpoint 1
                        |         disable display 1""".stripMargin
      override lazy val short = "dis"
      override val subcommands = Seq(DisableBreakpoint, DisableDisplay)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => throw new DebugException("one or more commands are required")
          case _ => {
            val subcmd = args.head
            val subcmdArgs = args.tail
            subcommands.find(_ == subcmd) match {
              case Some(c) => c(subcmdArgs, prestate, state, parser)
              case None => throw new DebugException("undefined disable command: %s".format(subcmd))
            }
          }
        }
        DebugState.Pause
      }

      object DisableBreakpoint extends DebugCommand {
        val name = "breakpoint"
        val desc = "disable a breakpoint"
        val longDesc = """|Usage: d[isable] b[reakpoint] <breakpoint_id>
                          |
                          |Disable a breakpoint with the specified id. This causes the breakpoint
                          |to be skipped during debugging.
                          |
                          |Example: disable breakpoint 1""".stripMargin
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("disable breakpoint command requires a single argument")
          }
          try {
            val id = args.head.toInt
            DebuggerConfig.breakpoints.find(_.id == id) match {
              case Some(b) => b.disable
              case None => throw new DebugException("%i is not a valid breakpoint id".format(id))
            }
          } catch {
            case _ => throw new DebugException("%s is not a valid breakpoint id".format(args.head))
          }
          DebugState.Pause
        }
      }

      object DisableDisplay extends DebugCommand {
        val name = "display"
        val desc = "disable a display"
        val longDesc = """|Usage: d[isable] di[splay] <display_id>
                          |
                          |Disable a display with the specified id. This causes the display command
                          |to be skipped during debugging.
                          |
                          |Example: disable display 1""".stripMargin
        override lazy val short = "di"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("disable display command requires a single argument")
          }
          try {
            val id = args.head.toInt
            DebuggerConfig.displays.find(_.id == id) match {
              case Some(d) => d.disable
              case None => throw new DebugException("%i is not a valid display id".format(id))
            }
          } catch {
            case _ => throw new DebugException("%s is not a valid display id".format(args.head))
          }
          DebugState.Pause
        }
      }
    }

    object Display extends DebugCommand {
      val name = "display"
      val desc = "show value of expression each time program stops"
      val longDesc = """|Usage: di[splay] <debugger_command>
                        |
                        |Execute a debugger command every time a debugger console is displayed
                        |to the user.
                        |
                        |Example: display info infoset""".stripMargin
      override lazy val short = "di"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.length == 0) {
          throw new DebugException("expression required for display command")
        } else {
          DebuggerConfig.displays += new Display(DebuggerConfig.displayIndex, args)
          DebuggerConfig.displayIndex += 1
        }
        DebugState.Pause
      }
    }

    object Enable extends DebugCommand {
      val name = "enable"
      val desc = "enable breakpoints and displays"
      val longDesc = """|Usage: e[nable] <type> <id>
                        |
                        |Enable a breakpoint or display. Valid values for <type> are "breakpoint"
                        |and "display".
                        |
                        |Example: enable breakpoint 1
                        |         enable display 1""".stripMargin
      override val subcommands = Seq(EnableBreakpoint, EnableDisplay)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => throw new DebugException("one or more commands are required")
          case _ => {
            val subcmd = args.head
            val subcmdArgs = args.tail
            subcommands.find(_ == subcmd) match {
              case Some(c) => c(subcmdArgs, prestate, state, parser)
              case None => throw new DebugException("undefined enable command: %s".format(subcmd))
            }
          }
        }
        DebugState.Pause
      }

      object EnableBreakpoint extends DebugCommand {
        val name = "breakpoint"
        val desc = "enable a breakpoint"
        val longDesc = """|Usage: e[nable] b[reakpoint] <breakpoint_id>
                          |
                          |Enable a breakpoint with the specified id. This causes the breakpoint
                          |to be evaluated during debugging.
                          |
                          |Example: enable breakpoint 1""".stripMargin
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("enable breakpoint command requires a single argument")
          }
          try {
            val id = args.head.toInt
            DebuggerConfig.breakpoints.find(_.id == id) match {
              case Some(b) => b.enable
              case None => throw new DebugException("%i is not a valid breakpoint id".format(id))
            }
          } catch {
            case _ => throw new DebugException("%s is not a valid breakpoint id".format(args.head))
          }
          DebugState.Pause
        }
      }

      object EnableDisplay extends DebugCommand {
        val name = "display"
        val desc = "enable a display"
        val longDesc = """|Usage: e[nable] di[splay] <display_id>
                          |
                          |Enable a display with the specified id. This causes the display command
                          |to be run during debugging.
                          |
                          |Example: enable display 1""".stripMargin
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("enable display command requires a single argument")
          }
          try {
            val id = args.head.toInt
            DebuggerConfig.displays.find(_.id == id) match {
              case Some(d) => d.enable
              case None => throw new DebugException("%i is not a valid display id".format(id))
            }
          } catch {
            case _ => throw new DebugException("%s is not a valid display id".format(args.head))
          }
          DebugState.Pause
        }
      }
    }

    object Eval extends DebugCommand {
      val name = "eval"
      val desc = "evaluate a DFDL expression"
      override lazy val short = "ev"
      val longDesc = """|Usage: ev[al] [type] <dfdl_expression>
                        |
                        |Evaluate a DFDL expression. The [type] argument determines how to
                        |display the result of the dfdl_expression. The value of [type] may
                        |be "string", "number", "boolean", or "node". If [type] is not given,
                        |then "node" is assumed."
                        |
                        |Example: eval number dfdl:occursIndex()""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.size == 0) {
          throw new DebugException("eval requires an optional type (string, number, node, boolean) and a DFDL expression")
        } else {
          val (evalType, expressionList) = args.head match {
            case "string" => (XPathConstants.STRING, args.tail)
            case "number" => (XPathConstants.NUMBER, args.tail)
            case "boolean" => (XPathConstants.BOOLEAN, args.tail)
            case "node" => (XPathConstants.NODE, args.tail)
            case _ => (XPathConstants.NODE, args) // assume node and all args are the expressoin
          }

          if (expressionList.size == 0) {
            throw new DebugException("DFDL expression required")
            return DebugState.Pause
          }

          try {
            DFDLFunctions.currentPState = Some(state)
            val expression = expressionList.mkString(" ")
            val compiledExpr = XPathUtil.compileExpression(expression, parser.context.namespaces, parser.context)
            val element = state.infoset.asInstanceOf[InfosetElement]
            val res = element.evalExpression(expression, compiledExpr, state.variableMap, evalType)
            res match {
              case NumberResult(n) => debugPrintln(n)
              case StringResult(s) => debugPrintln(s)
              case BooleanResult(b) => debugPrintln(b)
              case NodeResult(n) => {
                val xmlNode = XMLUtils.element2Elem(n)
                val xmlNoHidden =
                  if (DebuggerConfig.removeHidden) {
                    XMLUtils.removeHiddenElements(xmlNode)
                  } else {
                    xmlNode
                  }
                val xmlClean = XMLUtils.removeAttributes(xmlNoHidden(0), Seq(NS(XMLUtils.INT_NS)))
                val wrap = if (DebuggerConfig.wrapLength <= 0) Int.MaxValue else DebuggerConfig.wrapLength
                val pp = new scala.xml.PrettyPrinter(wrap, 2)
                val xml = pp.format(xmlClean)
                debugPrintln(xml)
              }
            }
          } catch {
            case e: XPathException => {
              val ex = if (e.getMessage() == null) e.getCause() else e
              throw new DebugException("expression evaluation failed: %s".format(ex))
            }
          } finally {
            DFDLFunctions.currentPState = None
          }
        }
        DebugState.Pause
      }
    }

    object Help extends DebugCommand {
      val name = "help"
      val desc = "display information about a command"
      val longDesc = """|Usage: h[elp] [command]
                        |
                        |Display help. If a command is given, display help information specific
                        |to that command and its subcommands.
                        |
                        |Example: help info""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        DebugCommandBase.help(args)
        DebugState.Pause
      }
    }

    object Info extends DebugCommand {
      val name = "info"
      val desc = "display information"
      val longDesc = """|Usage: i[nfo] <item>...
                        |
                        |Print internal information to the console. <item> can be specified
                        |multiple times to display multiple pieces of information.
                        |
                        |Example: info data infoset""".stripMargin
      override val subcommands = Seq(InfoArrayIndex, InfoBitLimit, InfoBitPosition, InfoBreakpoints, InfoChildIndex, InfoData, InfoDiff, InfoDiscriminator, InfoDisplays, InfoGroupIndex, InfoInfoset, InfoOccursCount, InfoParser, InfoPath)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => throw new DebugException("one or more commands are required")
          case _ => {
            args.foreach(arg => {
              subcommands.find(_ == arg) match {
                case Some(c) => c(Seq(), prestate, state, parser)
                case None => throw new DebugException("undefined info command: %s".format(arg))
              }
            })
          }
        }
        DebugState.Pause
      }

      object InfoArrayIndex extends DebugCommand {
        val name = "arrayIndex"
        override lazy val short = "ai"
        val desc = "display the current array limit"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.arrayPos != -1) {
            debugPrintln("%s: %d".format(name, state.arrayPos))
          } else {
            debugPrintln("%s: not in an array".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoBitLimit extends DebugCommand {
        val name = "bitLimit"
        override lazy val short = "bl"
        val desc = "display the current bit limit"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.bitLimit != -1) {
            debugPrintln("%s: %d".format(name, state.bitLimit))
          } else {
            debugPrintln("%s: no bit limit set".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoBitPosition extends DebugCommand {
        val name = "bitPosition"
        override lazy val short = "bp"
        val desc = "display the current bit position"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.bitPos != -1) {
            debugPrintln("%s: %d".format(name, state.bitPos))
          } else {
            debugPrintln("%s: no bit position set".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoBreakpoints extends DebugCommand {
        val name = "breakpoints"
        val desc = "display the current breakpoints"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (DebuggerConfig.breakpoints.size == 0) {
            debugPrintln("%s: no breakpoints set".format(name))
          } else {
            debugPrintln("%s:".format(name))
            DebuggerConfig.breakpoints.foreach { b =>
              {
                val enabledStr = if (b.enabled) "" else "*"
                debugPrintln("%s%s: %s   %s".format(b.id, enabledStr, b.breakpoint, b.condition.getOrElse("")), "  ")
              }
            }
          }
          DebugState.Pause
        }
      }

      object InfoChildIndex extends DebugCommand {
        val name = "childIndex"
        override lazy val short = "ci"
        val desc = "display the current child index"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.childPos != -1) {
            debugPrintln("%s: %d".format(name, state.childPos))
          } else {
            debugPrintln("%s: not a child element".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoData extends DebugCommand {
        val name = "data"
        val desc = "display the input data"
        val longDesc = desc

        val controlPicturesMapping = Map[String, String](
          ("\u0000", "\u2400"), // NULL
          ("\u0001", "\u2401"), // START OF HEADING
          ("\u0002", "\u2402"), // START OF TEXT
          ("\u0003", "\u2403"), // END OF TEXT
          ("\u0004", "\u2404"), // END OF TRANSMISSION
          ("\u0005", "\u2405"), // ENQUIRY
          ("\u0006", "\u2406"), // ACKNOWLEDGE
          ("\u0007", "\u2407"), // BELL
          ("\u0008", "\u2408"), // BACKSPACE
          ("\u0006", "\u2409"), // HORIZONTAL TABULATION
          ("\u000A", "\u240A"), // LINE FEED
          ("\u000B", "\u240B"), // VERTICAL TABULATION
          ("\u000C", "\u240C"), // FORM FEED
          ("\u000D", "\u240D"), // CARRIAGE RETURN
          ("\u000E", "\u240E"), // SHIFT OUT
          ("\u000F", "\u240F"), // SHIFT IN
          ("\u0010", "\u2410"), // DATA LINK ESCAPE
          ("\u0011", "\u2411"), // DEVICE CONTROL ONE
          ("\u0012", "\u2412"), // DEVICE CONTROL TWO
          ("\u0013", "\u2413"), // DEVICE CONTROL THREE
          ("\u0014", "\u2414"), // DEVICE CONTROL FOUR
          ("\u0015", "\u2415"), // NEGATIVE ACKNOWLEDGE
          ("\u0016", "\u2416"), // SYNCHRONOUS IDLE
          ("\u0017", "\u2417"), // END OF TRANSMISSION BLOCK
          ("\u0018", "\u2418"), // CANCEL
          ("\u0019", "\u2419"), // END OF MEDIUM
          ("\u001A", "\u241A"), // SUBSTITUTE
          ("\u001B", "\u241B"), // ESCAPE
          ("\u001C", "\u241C"), // FILE SEPARATOR
          ("\u001D", "\u241D"), // GROUP SEPARATOR
          ("\u001E", "\u241E"), // RECORD SEPARATOR
          ("\u001F", "\u241F"), // UNIT SEPARATOR
          //("\u0020", "\u2420"), // SPACE
          ("\u007F", "\u2421") // DELETE
          )

        def printData(l: Int, prestate: PState, state: PState, parser: Parser) {
          val length = if (l <= 0) Int.MaxValue - 1 else l
          parser.context match {
            case e: ElementBase => {
              if (e.representation == Representation.Text) {
                val dataLoc = prestate.currentLocation.asInstanceOf[DataLoc]
                val dumpLoc = ((prestate.bitPos >> 6) << 6) / 8;

                val numPrespaces = (prestate.charPos - dumpLoc).toInt
                val numSpaces = (state.charPos - dumpLoc).toInt

                val wrap = if (DebuggerConfig.wrapLength <= 0) Int.MaxValue else DebuggerConfig.wrapLength

                val formatStr =
                  if (numPrespaces == numSpaces) {
                    "│"
                  } else if (numPrespaces == numSpaces - 1) {
                    "║"
                  } else {
                    val parseLen = numSpaces - numPrespaces
                    val lineLength = wrap.min(length)
                    val barLen = parseLen.min(lineLength - numPrespaces)
                    val dashLen = barLen - 2
                    val base = "├" + ("─" * dashLen)
                    if (numPrespaces + parseLen > lineLength) {
                      base + "═"
                    } else {
                      base + "┤"
                    }
                  }
                val paddedFormatStr = "%s" + formatStr + " (%d to %d)".format(dumpLoc + numPrespaces, dumpLoc + numSpaces)

                val rawUtf8 = dataLoc.utf8Dump(length)
                val utf8 = controlPicturesMapping.foldLeft(rawUtf8) { (s, m) => s.replaceAll(m._1, m._2) }
                val lines = utf8.grouped(wrap)

                debugPrintln(paddedFormatStr.format(" " * numPrespaces), "  ")
                lines.foreach(l => debugPrintln(l, "  "))
              } else {
                debugPrintln(state.currentLocation.asInstanceOf[DataLoc].dump(length), "  ")
              }
            }
            case _ =>
          }
        }

        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s:".format(name))
          val len = if (args.size == 1) {
            try {
              args.head.toInt
            } catch {
              case _ => throw new DebugException("data length must be an integer")
            }
          } else {
            DebuggerConfig.dataLength
          }

          printData(len, prestate, state, parser)
          DebugState.Pause
        }
      }

      object InfoDiff extends DebugCommand {
        val name = "diff"
        override lazy val short = "diff"
        val desc = "display the differences from the previous state"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s:".format(name))
          if (prestate.bytePos != state.bytePos) debugPrintln("position (bytes): %d -> %d".format(prestate.bytePos, state.bytePos), "  ")
          if (prestate.bitLimit != state.bitLimit) debugPrintln("bitLimit: %d -> %d".format(prestate.bitLimit, state.bitLimit), "  ")
          if (prestate.discriminator != state.discriminator) debugPrintln("discriminator: %s -> %s".format(prestate.discriminator, state.discriminator), "  ")
          if (prestate.arrayPos != state.arrayPos) debugPrintln("arrayIndex: %d -> %d".format(prestate.arrayPos, state.arrayPos), "  ")
          if (prestate.groupPos != state.groupPos) debugPrintln("groupIndex: %d -> %d".format(prestate.groupPos, state.groupPos), "  ")
          if (prestate.childPos != state.childPos) debugPrintln("childIndex: %d -> %d".format(prestate.childPos, state.childPos), "  ")

          DebugState.Pause
        }
      }

      object InfoDiscriminator extends DebugCommand {
        val name = "discriminator"
        override lazy val short = "dis"
        val desc = "display whether or not a discriminator is set"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s: %b".format(name, state.discriminator))
          DebugState.Pause
        }
      }

      object InfoDisplays extends DebugCommand {
        val name = "displays"
        override lazy val short = "di"
        val desc = "display the current 'display' expressions"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (DebuggerConfig.displays.size == 0) {
            debugPrintln("%s: no displays set".format(name))
          } else {
            debugPrintln("%s:".format(name))
            DebuggerConfig.displays.foreach { d =>
              {
                val enabledStr = if (d.enabled) "" else "*"
                debugPrintln("%s%s: %s".format(d.id, enabledStr, d.cmd.mkString(" ")), "  ")
              }
            }
          }
          DebugState.Pause
        }
      }

      object InfoGroupIndex extends DebugCommand {
        val name = "groupIndex"
        override lazy val short = "gi"
        override val hidden = true
        val desc = "display the current group index"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.groupPos != -1) {
            debugPrintln("%s: %d".format(name, state.groupPos))
          } else {
            debugPrintln("%s: not in a group".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoInfoset extends DebugCommand {
        val name = "infoset"
        val desc = "display the current infoset"
        val longDesc = desc

        def getInfoset(currentNode: InfosetItem) = {
          val rootNode =
            if (currentNode.isInstanceOf[InfosetElement]) {
              var tmpNode = currentNode.asInstanceOf[InfosetElement]
              while (tmpNode.parent.isInstanceOf[InfosetElement]) {
                tmpNode = tmpNode.parent.asInstanceOf[InfosetElement]
              }
              tmpNode
            } else {
              currentNode
            }

          val xmlNode = rootNode.toXML
          val xmlNoHidden = if (DebuggerConfig.removeHidden) {
            XMLUtils.removeHiddenElements(xmlNode)
          } else {
            xmlNode
          }
          val xmlClean = XMLUtils.removeAttributes(xmlNoHidden(0), Seq(NS(XMLUtils.INT_NS)))
          xmlClean
        }

        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          val infoset = getInfoset(state.infoset)
          val wrap = if (DebuggerConfig.wrapLength <= 0) Int.MaxValue else DebuggerConfig.wrapLength
          val pp = new scala.xml.PrettyPrinter(wrap, 2)
          debugPrintln("%s:".format(name))
          val xml = pp.format(infoset)
          val lines = xml.split("\n")
          val tailLines =
            if (DebuggerConfig.infosetLines > 0) {
              val dropCount = lines.size - DebuggerConfig.infosetLines
              if (dropCount > 0) {
                debugPrintln("...", "  ")
              }
              lines.drop(dropCount)
            } else {
              lines
            }
          tailLines.foreach(l => debugPrintln(l, "  "))

          DebugState.Pause
        }
      }

      object InfoOccursCount extends DebugCommand {
        val name = "occursCount"
        override lazy val short = "oc"
        val desc = "display the current occurs count"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.occursCount != -1) {
            debugPrintln("%s: %d".format(name, state.occursCount))
          } else {
            debugPrintln("%s: occurs count not set".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoParser extends DebugCommand {
        val name = "parser"
        val desc = "display the current Daffodil parser"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s: %s".format(name, parser.toString))
          DebugState.Pause
        }
      }

      object InfoPath extends DebugCommand {
        val name = "path"
        override lazy val short = "path"
        val desc = "display the current schema component designator/path"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s: %s".format(name, parser.context.path))
          DebugState.Pause
        }
      }
    }

    object Quit extends DebugCommand {
      val name = "quit"
      val desc = "immediately abort all processing"
      val longDesc = """|Usage: q[uit]
                        |
                        |Immediately abort all processing.""".stripMargin
      override lazy val short = "q"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        sys.exit(1)
      }
    }

    object Set extends DebugCommand {
      val name = "set"
      val desc = "modify debugger configuration"
      val longDesc = """|Usage: set <setting> <value>
                        |
                        |Change a debugger setting, the list of settings are below.""".stripMargin
      override val subcommands = Seq(SetBreakOnFailure, SetBreakOnlyOnCreation, SetDataLength, SetInfosetLines, SetRemoveHidden, SetWrapLength)
      override lazy val short = "set"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => throw new DebugException("a subcommand is reuiqred")
          case _ => {
            val subcmd = args.head
            val subcmdArgs = args.tail
            subcommands.find(_ == subcmd) match {
              case Some(c) => c(subcmdArgs, prestate, state, parser)
              case None => throw new DebugException("undefined set command: %s".format(subcmd))
            }
          }
        }
        DebugState.Pause
      }

      object SetBreakOnlyOnCreation extends DebugCommand {
        val name = "breakOnlyOnCreation"
        val desc = "whether or not breakpoints should occur only on element creation, or always"
        val longDesc = """|Usage: s[et] breakOnlyOnCreation|booc <value>
                          |
                          |Set whether or not breakpoints should only be evaluated on element creation.
                          |<value> must be either true/false or 1/0. If true, breakpoints only stop on
                          |element creation. If false, breakpoints stop anytime a parser interacts with
                          |an element. Defaults to true.
                          |
                          |Example: set breakOnlyOnCreation false""".stripMargin
        override lazy val short = "booc"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            val state = args.head
            DebuggerConfig.breakOnlyOnCreation =
              if (state == "true" || state == "1") {
                true
              } else if (state == "false" || state == "0") {
                false
              } else {
                throw new DebugException("argument must be true/false or 1/0")
              }
          }
          DebugState.Pause
        }
      }

      object SetBreakOnFailure extends DebugCommand {
        val name = "breakOnFailure"
        val desc = "whether or not stop break on failures"
        val longDesc = """|Usage: s[et] breakOnFailure|bof <value>
                          |
                          |Set whether or the debugger should break on failures. If set to false
                          |the normal processing occurs. If set to true, any errors cause a break.
                          |Note that due to the backtracking behavior, not all failures are fatal.
                          |Defaults to true.
                          |
                          |Example: set breakOnFailure false""".stripMargin
        override lazy val short = "bof"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            val state = args.head
            DebuggerConfig.breakOnFailure =
              if (state == "true" || state == "1") {
                true
              } else if (state == "false" || state == "0") {
                false
              } else {
                throw new DebugException("argument must be true/false or 1/0")
              }
          }
          DebugState.Pause
        }
      }

      object SetDataLength extends DebugCommand {
        val name = "dataLength"
        val desc = "set the maximum number of bytes of the data to display"
        val longDesc = """|Usage: s[et] dataLength|dl <value>
                          |
                          |Set the number of bytes to display when display input data. If negative,
                          |display all input data. This only affects the 'info data' command.
                          |Defaults to 70 bytes.
                          |
                          |Example: set dataLength 100
                          |         set dataLength -1""".stripMargin
        override lazy val short = "dl"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            try {
              val len = args.head.toInt
              DebuggerConfig.dataLength = len
            } catch {
              case _ => throw new DebugException("an integer argument is required")
            }
          }
          DebugState.Pause
        }
      }

      object SetInfosetLines extends DebugCommand {
        val name = "infosetLines"
        val desc = "set the maximum number of lines of the infoset to display"
        val longDesc = """|Usage: s[et] infosetLines|il <value>
                          |
                          |Set the maximum number of lines to display when displaying the infoset.
                          |This only affects the 'info infoset' command. This shows the last
                          |<value> lines of the infoset. If <value> is less than or equal to zero,
                          |the entire infoset is printed. Defaults to -1.
                          |
                          |Example: set infosetLines 25""".stripMargin
        override lazy val short = "il"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            try {
              val len = args.head.toInt
              DebuggerConfig.infosetLines = len
            } catch {
              case _ => throw new DebugException("an integer argument is required")
            }
          }
          DebugState.Pause
        }
      }

      object SetRemoveHidden extends DebugCommand {
        val name = "removeHidden"
        val desc = "set whether or not to remove Daffodil internal attributes when displaying the infoset (true/false or 1/0)"
        val longDesc = """|Usage: s[et] removeHidden|rh <value>
                          |
                          |Set whether or not hidden elements (e.g through the use of
                          |dfdl:hiddenGroupRef) should be displayed. This effects the 'eval' and
                          |'info infoset' commands. <value> must be either true/false or 1/0.
                          |Defaults to false.
                          |
                          |Example: set removeHidden true""".stripMargin
        override lazy val short = "rh"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            val state = args.head
            DebuggerConfig.removeHidden =
              if (state == "true" || state == "1") {
                true
              } else if (state == "false" || state == "0") {
                false
              } else {
                throw new DebugException("argument must be true/false or 1/0")
              }

          }
          DebugState.Pause
        }
      }

      object SetWrapLength extends DebugCommand {
        val name = "wrapLength"
        val desc = "set the maximum number of bytes to display before wrapping"
        val longDesc = """|Usage: s[et] wrapLength|wl <value>
                          |
                          |Set the number of characters at which point output wraps. This only
                          |affects the 'info data' and 'info infoset' commands. A length less
                          |than or equal to zero disables wrapping. Defaults to 80 characters.
                          |
                          |Example: set wrapLength 100
                          |         set wrapLength -1""".stripMargin
        override lazy val short = "wl"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            try {
              val len = args.head.toInt
              DebuggerConfig.wrapLength = len
            } catch {
              case _ => throw new DebugException("an integer argument is required")
            }
          }
          DebugState.Pause
        }
      }
    }

    object Step extends DebugCommand {
      val name = "step"
      val desc = "execute a single parser step"
      val longDesc = """|Usage: s[tep]
                        |
                        |Perform a single parse action, pause parsing, and display a debugger
                        |prompt.""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        DebugState.Step
      }
    }

    object Trace extends DebugCommand {
      val name = "trace"
      val desc = "same as continue, but runs display commands during every step"
      val longDesc = """|Usage: t[race]
                        |
                        |Continue parsing the input data until a breakpoint is encountered,
                        |while running display commands after every parse step. When a
                        |breakpoint is encountered, pause parsing and display a debugger
                        |console to the user.""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        DebugState.Trace
      }
    }

    object UnDisplay extends DebugCommand {
      val name = "undisplay"
      val desc = "remove display"
      val longDesc = """|Usage: u[ndisplay] <display_id>
                        |
                        |Remove a display created using the 'display' command.
                        |
                        |Example: undisplay 1""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.length != 1) {
          throw new DebugException("undisplay command requires a single argument")
        }

        try {
          val id = args.head.toInt
          DebuggerConfig.displays.find(d => d.id == id) match {
            case Some(d) => DebuggerConfig.displays -= d
            case None => throw new DebugException("display %i not found".format(id))
          }
        } catch {
          case _ => throw new DebugException("not a valid display id")
        }
        DebugState.Pause
      }
    }
  }
}
