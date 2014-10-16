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
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.ExecutionMode
import java.io.File
import jline.console.completer.Completer
import jline.console.completer.StringsCompleter
import jline.console.completer.AggregateCompleter
import edu.illinois.ncsa.daffodil.util.Enum
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import javax.xml.xpath.XPathException
import edu.illinois.ncsa.daffodil.dpath._

abstract class InteractiveDebuggerRunner {
  def init(id: InteractiveDebugger): Unit
  def getCommand: String
  def lineOutput(line: String): Unit
  def fini(): Unit
}

class InteractiveDebugger(runner: InteractiveDebuggerRunner) extends Debugger {

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
    var breakOnFailure: Boolean = false

    /* list of breakpoints */
    val breakpoints = collection.mutable.ListBuffer[Breakpoint]()
    var breakpointIndex: Int = 1

    /* list of displays */
    val displays = collection.mutable.ListBuffer[Display]()
    var displayIndex: Int = 1

    /* whether to remove hidden elements when displaying the infoset */
    var removeHidden: Boolean = false

    /* stores the last actual command (i.e. not a "") that was executed */
    var lastCommand: String = ""

    /* stores the full list of commands as typed, even blanks. We need to
     * maintain our own because the jline history ignores empty commands and
     * duplicate commands. That can be configured, but jline's history is much
     * more useful with the history behaving that way. This is really only used
     * for the 'history' command. */
    val history = scala.collection.mutable.ListBuffer[String]()

    /* keeps track of which parse step we're on for trace output */
    var parseStep = 0

    /* how to display data */
    var representation: Representation.Value = Representation.Text
  }

  var debugState: DebugState.Type = DebugState.Pause

  override def init(parser: Parser) {
    runner.init(this)
  }

  override def fini(parser: Parser) {
    runner.fini
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
        DebuggerConfig.displays.filter(_.enabled).foreach { d => runCommand(d.cmd, before, after, parser) }

        if (after.status != Success) {
          debugPrintln("failure:")
          debugPrintln("%s".format(after.diagnostics.head.getMessage), "  ")
        }

        if (debugState == DebugState.Trace) {
          debugPrintln("----------------------------------------------------------------- " + DebuggerConfig.parseStep)
        }
      }

      DebuggerConfig.parseStep += 1

      while (debugState == DebugState.Pause) {
        val args = readCmd
        debugState = runCommand(args, before, after, parser)
      }
    }
  }

  private def isInteresting(parser: Parser): Boolean = true
  //  {
  //    parser.toString match {
  //      case "Sequence" => false
  //      case "ComplexType" => false
  //      case _ => {
  //        if (parser.toString.startsWith("<seq>")) {
  //          false
  //        } else if (parser.toString.startsWith("RepExactlyN")) {
  //          false
  //        } else if (parser.toString.startsWith("RepAtMostTotalN")) {
  //          false
  //        } else {
  //          true
  //        }
  //      }
  //    }
  //  }

  private def readCmd(): Seq[String] = {
    val input = runner.getCommand.trim

    DebuggerConfig.history += input

    val cmd = input match {
      case "" => {
        DebuggerConfig.lastCommand
      }
      case _ => {
        DebuggerConfig.lastCommand = input
        input
      }
    }
    cmd.split(" ").filter(_ != "")
  }

  private def evaluateBooleanExpression(expression: String, state: PState, parser: Parser): Boolean = {
    val context = state.getContext()
    val namespaces = context.namespaces
    val compiledExpr = ExpressionCompiler.compileExpression(
      NodeInfo.Boolean, expression, parser.context.namespaces, context.dpathCompileInfo)
    try {
      val (res, vars) = compiledExpr.evaluate(state)
      res match {
        case b: Boolean => b
        case _ => false
      }
    } catch {
      case e: XPathException => false
    }
  }

  private def findBreakpoint(state: PState, parser: Parser): Option[Breakpoint] = {
    // TODO: detecting if the parser is an ElementBegin parser is a little
    // tricky because of anonymous parsers. So solve this, make sure the parser
    // is an instance of PrimParser, then check if the primitive representing
    // the prim parser is an instance of ElementBegin. This should be changed
    // if we ever get rid of anonymous parsers.
    //
    // UPDATE:  ElementBegin parser was removed during update to RuntimeData and ElementRuntimeData
    //
    // Note: we use strings and getClass.getName here to get late binding. The ElementBegin class isn't defined
    // in the same module this is. (Layering structure issue.)
    //
    if (!DebuggerConfig.breakOnlyOnCreation) {
      val foundBreakpoint =
        DebuggerConfig.breakpoints
          .filter(_.enabled)
          .filter { bp =>
            bp.breakpoint == parser.context.prettyName ||
              bp.breakpoint == parser.context.path ||
              "element." + bp.breakpoint == parser.context.prettyName
          }
          .find { bp =>
            bp.condition match {
              case Some(expression) => evaluateBooleanExpression(expression, state, parser)
              case None => true
            }
          }
      foundBreakpoint
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
        val out = "%s%s".format(prefix, line)
        runner.lineOutput(out)
      }
    }
  }

  private def debugPrettyPrintXML(ie: InfosetElement) {
    val xml = if (DebuggerConfig.removeHidden) {
      ie.removeHiddenElements().toXML
    } else {
      ie.toXML
    }
    if (xml.length == 0) {
      debugPrintln("Nil"); return
    }
    Assert.invariant(xml.length == 1)
    val elem = xml(0)
    val xmlClean = XMLUtils.removeAttributes(elem) // strip them all , Seq(XMLUtils.INT_NS_OBJECT))
    val wrap = if (DebuggerConfig.wrapLength <= 0) Int.MaxValue else DebuggerConfig.wrapLength
    val pp = new scala.xml.PrettyPrinter(wrap, 2)
    val xmlString = pp.format(xmlClean)
    debugPrintln(xmlString)
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

      // this wraps a line of text to a maximum width, breaking on space
      def wrapLine(line: String, width: Int): List[String] = {
        if (line.length == 0) {
          Nil
        } else {
          val spaceIndex = line.lastIndexOf(" ", width)
          if (line.length < width || spaceIndex == -1) {
            List(line)
          } else {
            val wrapped = line.take(spaceIndex) :: wrapLine(line.drop(spaceIndex + 1), width)
            wrapped
          }
        }
      }

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
          visibleSubcommands.foreach(c => {
            val descColumnWidth = 75
            val descLines = wrapLine(c.desc, descColumnWidth - maxLen)
            val prefixes = c.name :: List.fill(descLines.length - 1)("")
            prefixes.zip(descLines).foreach { case (p, d) => debugPrintln(formatString.format(p, d)) }
          })
        }
        case _ => {
          val subcmd = args.head
          val subcmdArgs = args.tail
          subcommands.find(_ == subcmd) match {
            case Some(cmd) => cmd.help(subcmdArgs)
            case None => throw new DebugException("unknown command: %s".format(subcmd))
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
            case _ => Assert.impossible("cmd/args were split incorrectly")
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
    // be run whenever names change or new commands are added.

    // Uncomment this and the DebugCommandBase.checkNameConflicts line to do a
    // check when changes are made.
    //
    /*
    def checkNameConflicts() {
      val allNames = subcommands.map(_.name) ++ subcommands.filter{ sc => sc.name != sc.short }.map(_.short)
      val duplicates = allNames.groupBy{ n => n }.filter{ case(_, l) => l.size > 1 }.keys
      if (duplicates.size > 0) {
        Assert.invariantFailed("Duplicate debug commands found in '%s' command: ".format(name) + duplicates)
      }
      subcommands.foreach(_.checkNameConflicts)
    }
    */
  }

  //DebugCommandBase.checkNameConflicts

  object DebugCommandBase extends DebugCommand {
    val name = ""
    val desc = ""
    val longDesc = ""
    override lazy val short = ""
    override val subcommands = Seq(Break, Clear, Complete, Condition, Continue, Delete, Disable, Display, Enable, Eval, Help, History, Info, Quit, Set, Step, Trace)
    def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
      val newState = args.size match {
        case 0 => {
          throw new DebugException("no command specified")
        }
        case _ => {
          val subcmd = args.head
          val subcmdArgs = args.tail
          subcommands.find(_ == subcmd) match {
            case Some(c) => c(subcmdArgs, prestate, state, parser)
            case None => {
              throw new DebugException("undefined command: %s".format(subcmd))
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
          val bp = new Breakpoint(DebuggerConfig.breakpointIndex, args.head)
          DebuggerConfig.breakpoints += bp
          debugPrintln("%s: %s".format(bp.id, bp.breakpoint))
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
        }
        try {
          val id = args.head.toInt
          val expression = args.tail.mkString(" ")
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => {
              b.condition = Some(expression)
              debugPrintln("%s: %s   %s".format(b.id, b.breakpoint, expression))
            }
            case None => throw new DebugException("breakpoint %i not found".format(id))
          }
        } catch {
          case _: Throwable => throw new DebugException("not a valid breakpoint id")
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
      val desc = "delete breakpoints and displays"
      val longDesc = """|Usage: d[elete] <type> <id>
                        |
                        |Remove a breakpoint or display. Valid values for <type> are "breakpoint"
                        |and "display".
                        |
                        |Example: delete breakpoint 1
                        |         delete display 1""".stripMargin
      override val subcommands = Seq(DeleteBreakpoint, DeleteDisplay)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => throw new DebugException("one or more commands are required")
          case _ => {
            val subcmd = args.head
            val subcmdArgs = args.tail
            subcommands.find(_ == subcmd) match {
              case Some(c) => c(subcmdArgs, prestate, state, parser)
              case None => throw new DebugException("undefined delete command: %s".format(subcmd))
            }
          }
        }
        DebugState.Pause
      }

      object DeleteBreakpoint extends DebugCommand {
        val name = "breakpoint"
        val desc = "delete a breakpoint"
        val longDesc = """|Usage: d[elete] b[reakpoint] <breakpoint_id>
                          |
                          |Remove a breakpoint created using the 'breakpoint' command.
                          |
                          |Example: delete breakpoint 1""".stripMargin
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("delete breakpoint command requires a single argument")
          }
          try {
            val id = args.head.toInt
            DebuggerConfig.breakpoints.find(_.id == id) match {
              case Some(b) => DebuggerConfig.breakpoints -= b
              case None => throw new DebugException("breakpoint %i not found".format(id))
            }
          } catch {
            case _: Throwable => throw new DebugException("not a valid breakpoint id")
          }
          DebugState.Pause
        }
      }

      object DeleteDisplay extends DebugCommand {
        val name = "display"
        val desc = "delete a display"
        val longDesc = """|Usage: d[elete] di[splay] <display_id>
                          |
                          |Remove a display created using the 'display' command.
                          |
                          |Example: delete display 1""".stripMargin
        override lazy val short = "di"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.length != 1) {
            throw new DebugException("delete display command requires a single argument")
          }

          try {
            val id = args.head.toInt
            DebuggerConfig.displays.find(d => d.id == id) match {
              case Some(d) => DebuggerConfig.displays -= d
              case None => throw new DebugException("display %i not found".format(id))
            }
          } catch {
            case _: Throwable => throw new DebugException("not a valid display id")
          }
          DebugState.Pause
        }
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
        val longDesc = """|Usage: dis[able] b[reakpoint] <breakpoint_id>
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
            case _: Throwable => throw new DebugException("%s is not a valid breakpoint id".format(args.head))
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
            case _: Throwable => throw new DebugException("%s is not a valid display id".format(args.head))
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
            case _: Throwable => throw new DebugException("%s is not a valid breakpoint id".format(args.head))
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
            case _: Throwable => throw new DebugException("%s is not a valid display id".format(args.head))
          }
          DebugState.Pause
        }
      }
    }

    object Eval extends DebugCommand {
      val name = "eval"
      val desc = "evaluate a DFDL expression"
      override lazy val short = "ev"
      val longDesc = """|Usage: ev[al] <dfdl_expression>
                        |
                        |Evaluate a DFDL expression. 
                        |
                        |Example: eval dfdl:occursIndex()""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        if (args.size == 0) {
          throw new DebugException("eval requires a DFDL expression")
        }
        val expressionList = args
        val expression = expressionList.mkString(" ")

        val element = state.infoset match {
          case e: InfosetElement => e
          case d: InfosetDocument => d.getRootElement()
        }
        // this adjustment is so that automatic display of ".." doesn't fail
        // for the root element.
        val adjustedExpression =
          if (!element.parent.isDefined && (expression == "..")) "."
          else expression
        val context = state.getContext()
        val namespaces = context.namespaces
        val expressionWithBraces =
          if (!DPathUtil.isExpression(adjustedExpression)) "{ " + adjustedExpression + " }"
          else adjustedExpression
        val isEvaluatedAbove = false
        val compiledExpression = ExpressionCompiler.compile(
          NodeInfo.AnyType, expressionWithBraces, namespaces, context.dpathCompileInfo,
          isEvaluatedAbove)
        try {

          val (res, vmap) = compiledExpression.evaluate(state)
          res match {
            case ie: InfosetElement => debugPrettyPrintXML(ie)
            case nodeSeq: Seq[Any] => nodeSeq.foreach { a =>
              a match {
                case ie: InfosetElement => debugPrettyPrintXML(ie)
                case _ => debugPrintln(a)
              }
            }
            case _ => debugPrintln(res)
          }
        } catch {
          case e: XPathException => {
            val ex = if (e.getMessage() == null) e.getCause() else e
            throw new DebugException("expression evaluation failed: %s".format(ex))
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

    object History extends DebugCommand {
      val name = "history"
      override lazy val short = "hi"
      val desc = "display the history of commands"
      val longDesc = """|Usage: hi[story] [outfile]
                        |
                        |Display the history of commands. If an argument is given, write
                        |the history to the specified file rather then printing it to the
                        |screen.
                        |
                        |Example: history
                        |         history out.txt""".stripMargin
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
        args.size match {
          case 0 => {
            debugPrintln("%s:".format(name))
            DebuggerConfig.history.zipWithIndex.foreach { case (cmd, index) => debugPrintln("%d: %s".format(index, cmd), "  ") }

          }
          case 1 => {
            try {
              val path =
                if (args.head.startsWith("~" + File.separator)) {
                  System.getProperty("user.home") + args.head.substring(1)
                } else {
                  args.head
                }
              val fw = new java.io.FileWriter(path)
              val bw = new java.io.BufferedWriter(fw)
              // use .init to drop the 'history outfile' command, we want
              // something that can be easily provided to the InteractiveDebugger constructor
              DebuggerConfig.history.init.foreach(cmd => {
                bw.write(cmd)
                bw.newLine()
              })
              bw.close()
              fw.close()
              debugPrintln("%s: written to %s".format(name, args.head))
            } catch {
              case e: Throwable => throw new DebugException("failed to write history file: " + e.getMessage)
            }
          }
          case _ => throw new DebugException("too many arguments specified")
        }
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
      override val subcommands = Seq(InfoArrayIndex, InfoBitLimit, InfoBitPosition, InfoBreakpoints, InfoChildIndex, InfoData, InfoDiff, InfoDiscriminator, InfoDisplays, InfoGroupIndex, InfoInfoset, InfoOccursBounds, InfoParser, InfoPath)
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
          if (state.mpstate.arrayPos != -1) {
            debugPrintln("%s: %d".format(name, state.mpstate.arrayPos))
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
          if (state.bitLimit0b != -1) {
            debugPrintln("%s: %d".format(name, state.bitLimit0b))
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
          if (state.mpstate.childPos != -1) {
            debugPrintln("%s: %d".format(name, state.mpstate.childPos))
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

        def printData(rep: Representation.Value, l: Int, prestate: PState, state: PState, parser: Parser) {
          val length = if (l <= 0) Int.MaxValue - 1 else l

          val infoset = state.infoset

          rep match {
            case Representation.Text => {
              val dataLoc = prestate.currentLocation.asInstanceOf[DataLoc]
              val dumpLoc = ((prestate.bitPos >> 6) << 6) / 8;

              val numPrespaces = (math.max(prestate.charPos, 0L) - dumpLoc).toInt
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
              val utf8 = Misc.remapControlsAndLineEndingsToVisibleGlyphs(rawUtf8)
              val lines = utf8.grouped(wrap)

              debugPrintln(paddedFormatStr.format(" " * numPrespaces), "  ")
              lines.foreach(l => debugPrintln(l, "  "))
            }

            case Representation.Binary => {
              debugPrintln(state.currentLocation.asInstanceOf[DataLoc].dump(length), "  ")
            }
          }
        }

        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          debugPrintln("%s:".format(name))
          val rep = if (args.size > 0) {
            args(0).toLowerCase match {
              case "t" => Representation.Text
              case "b" => Representation.Binary
              case _ => throw new DebugException("uknown representation: %s. Must be one of 't' for text or 'b' for binary".format(args(0)))
            }
          } else {
            DebuggerConfig.representation
          }

          val len = if (args.size > 1) {
            try {
              args(1).toInt
            } catch {
              case _: Throwable => throw new DebugException("data length must be an integer")
            }
          } else {
            DebuggerConfig.dataLength
          }

          printData(rep, len, prestate, state, parser)
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
          var diff = false
          if (prestate.bytePos != state.bytePos) { debugPrintln("position (bytes): %d -> %d".format(prestate.bytePos, state.bytePos), "  "); diff = true }
          if (prestate.bitLimit0b != state.bitLimit0b) { debugPrintln("bitLimit: %d -> %d".format(prestate.bitLimit0b, state.bitLimit0b), "  "); diff = true }
          if (prestate.discriminator != state.discriminator) { debugPrintln("discriminator: %s -> %s".format(prestate.discriminator, state.discriminator), "  "); diff = true }
          if (prestate.mpstate.arrayPos != state.mpstate.arrayPos) { debugPrintln("arrayIndex: %d -> %d".format(prestate.mpstate.arrayPos, state.mpstate.arrayPos), "  "); diff = true }
          if (prestate.mpstate.groupPos != state.mpstate.groupPos) { debugPrintln("groupIndex: %d -> %d".format(prestate.mpstate.groupPos, state.mpstate.groupPos), "  "); diff = true }
          if (prestate.mpstate.childPos != state.mpstate.childPos) { debugPrintln("childIndex: %d -> %d".format(prestate.mpstate.childPos, state.mpstate.childPos), "  "); diff = true }

          if (diff == false) {
            debugPrintln("No differences", "  ")
          }

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
        val desc = "display the current group index"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.mpstate.groupPos != -1) {
            debugPrintln("%s: %d".format(name, state.mpstate.groupPos))
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

              var tmpNode = currentNode.asInstanceOf[DIElement]
              var parent = tmpNode.diParent
              while (parent.isDefined) {
                tmpNode = parent.get
                parent = tmpNode.diParent
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
          val xmlClean = XMLUtils.removeAttributes(xmlNoHidden(0)) //, Seq(XMLUtils.INT_NS_OBJECT))
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

      object InfoOccursBounds extends DebugCommand {
        val name = "occursBounds"
        override lazy val short = "oc"
        val desc = "display the current occurs bounds"
        val longDesc = desc
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (state.mpstate.occursBounds != -1) {
            debugPrintln("%s: %d".format(name, state.mpstate.occursBounds))
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
          debugPrintln("%s: %s".format(name, parser.toBriefXML(2))) // only 2 levels of output, please!
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
                        |Change a debugger setting, the list of settings are below.
                        |
                        |Example: set breakOnlyOnCreate false
                        |         set dataLength 100""".stripMargin
      override val subcommands = Seq(SetBreakOnFailure, SetBreakOnlyOnCreation, SetDataLength, SetInfosetLines, SetRemoveHidden, SetRepresentation, SetWrapLength)
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
        val desc = "whether or not breakpoints should occur only on element creation, or always (default: true)"
        val longDesc = """|Usage: set breakOnlyOnCreation|booc <value>
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
        val desc = "whether or not stop break on failures (default: false)"
        val longDesc = """|Usage: set breakOnFailure|bof <value>
                          |
                          |Set whether or the debugger should break on failures. If set to false
                          |the normal processing occurs. If set to true, any errors cause a break.
                          |Note that due to the backtracking behavior, not all failures are fatal.
                          |Defaults to false.
                          |
                          |Example: set breakOnFailure true""".stripMargin
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
        val desc = "set the maximum number of bytes of the data to display. If negative, display all input data (default: 70)"
        val longDesc = """|Usage: set dataLength|dl <value>
                          |
                          |Set the number of bytes to display when displaying input data. If
                          |negative, display all input data. This only affects the 'info data'
                          |command. Defaults to 70 bytes.
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
              case _: Throwable => throw new DebugException("an integer argument is required")
            }
          }
          DebugState.Pause
        }
      }

      object SetInfosetLines extends DebugCommand {
        val name = "infosetLines"
        val desc = "set the maximum number of lines of the infoset to display (default: -1)"
        val longDesc = """|Usage: set infosetLines|il <value>
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
              case _: Throwable => throw new DebugException("an integer argument is required")
            }
          }
          DebugState.Pause
        }
      }

      object SetRemoveHidden extends DebugCommand {
        val name = "removeHidden"
        val desc = "set whether or not to remove Daffodil internal attributes when displaying the infoset (default: false)"
        val longDesc = """|Usage: set removeHidden|rh <value>
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

      object SetRepresentation extends DebugCommand {
        val name = "representation"
        val desc = "set the output when displaying data (default: text)"
        val longDesc = """|Usage: set representation|rp <value>
                          |
                          |Set the output when displaying data. <value> must be either
                          |'text' or 'binary'. Defaults to 'text'.
                          |Defaults to false.
                          |
                          |Example: set representation binary""".stripMargin
        override lazy val short = "rp"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.Type = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            DebuggerConfig.representation = args.head.toLowerCase match {
              case "text" => Representation.Text
              case "binary" => Representation.Binary
              case _ => throw new DebugException("argument must be either 'text' or 'binary'")
            }
          }
          DebugState.Pause
        }
      }

      object SetWrapLength extends DebugCommand {
        val name = "wrapLength"
        val desc = "set the maximum number of bytes to display before wrapping (default: 80)"
        val longDesc = """|Usage: set wrapLength|wl <value>
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
              case _: Throwable => throw new DebugException("an integer argument is required")
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
  }
}
