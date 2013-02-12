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
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil
import edu.illinois.ncsa.daffodil.processors.xpath.{NumberResult, StringResult, BooleanResult, NodeResult, DFDLFunctions}
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS

import javax.xml.xpath.{XPathConstants, XPathException}

import jline.console.ConsoleReader
import jline.console.completer.Completer
import jline.console.completer.StringsCompleter
import jline.console.completer.AggregateCompleter

class InteractiveDebugger extends Debugger {

  object DebugState extends Enumeration {
    type DebugState = Value
    val Continue, Step, Pause = Value
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

    /* list of breakpoints */
    val breakpoints = collection.mutable.ListBuffer[Breakpoint]()
    var breakpointIndex: Int = 1

    /* list of displays */
    val displays = collection.mutable.ListBuffer[Display]()
    var displayIndex: Int = 1

    /* whether to remove hidden elements when displaying the infoset */
    var removeHidden: Boolean = false
  }
   
  var debugState = DebugState.Pause

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

    debugState = debugState match {
      case DebugState.Continue => {
        findBreakpoint(after, parser) match {
          case Some(bp) => DebugState.Pause
          case None => debugState
        }
      }
      case DebugState.Step => DebugState.Pause
      case _ => debugState
    }

    if (debugState == DebugState.Pause) {
      DebuggerConfig.displays.filter(_.enabled).foreach { d => runCommand(d.cmd, before, after, parser) }
    }

    while (debugState == DebugState.Pause) {
      val args = readCmd
      debugState = runCommand(args, before, after, parser)
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
      case _ => input
    }
    cmd.trim.split(" ").filter(_ != "")
  }

  private def expressionIsTrue(expression: String, state: PState, parser: Parser): Boolean = {
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
    if (parser.toString.startsWith("<ElementBegin")) {
      DebuggerConfig.breakpoints.filter(_.enabled).find { bp => bp.breakpoint == parser.context.prettyName || bp.breakpoint == parser.context.path  } match {
        case Some(bp) => {
          bp.condition match {
            case Some(expression) => if (expressionIsTrue(expression, state, parser)) Some(bp) else None
            case None => Some(bp)
          }
        }
        case None => None
      }
    } else {
      None
    }
  }

  private def runCommand(cmd: Seq[String], before: PState, after: PState, parser: Parser): DebugState.DebugState = {
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
    obj.toString.split("\n").foreach { line => {
      print("  ")
      print(prefix)
      println(line)
    }}
  }

  /**********************************/
  /**          Commands            **/
  /**********************************/
  
  abstract class DebugCommand {
    val name: String
    lazy val short: String = name(0).toString
    val desc: String
    val subcommands: Seq[DebugCommand] = Seq()

    def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState

    override def equals(that: Any): Boolean = {
      that match {
        case str: String => (str == name || str == short)
        case _ => super.equals(that)
      }
    }

    def help(args: Seq[String]): Unit = {
      args.length match {
        case 0 => {
          if (name != "") {
            debugPrintln("%s  %s".format(name, desc))
            if (!subcommands.isEmpty) {
              debugPrintln()
              debugPrintln("subcommands:")
              debugPrintln()
            }
          }
          val maxLen = subcommands.foldLeft(0) { (i, c) => i.max(c.name.length) }
          val formatString = "%-" + maxLen + "s  %s"
          subcommands.foreach(c => debugPrintln(formatString.format(c.name, c.desc)))
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
      val subcommandsCompleter = new AggregateCompleter(dc.subcommands.sortBy(_.name).map(_.completer):_*)
      def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {
        val cmds = buffer.replaceAll("^\\s+", "").split("(?= )", 2).toList
        val (cmd, args) = cmds match {
          case c::rest => rest match {
            case a::Nil => (c, a)
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
  }


  object DebugCommandBase extends DebugCommand {
    val name = ""
    val desc = ""
    override lazy val short = ""
    override val subcommands = Seq(Break, Clear, Condition, Continue, Delete, Disable, Display, Enable, Eval, Help, Info, Set, Step, UnDisplay)
    def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
              throw new DebugException("undefined info command: %s".format(subcmd))
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
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
      override lazy val short = "cl"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
        print(27: Char)
        print('[')
        print("2J")
        print(27: Char)
        print('[')
        print("1;1H")

        DebugState.Pause
      }
    }

    object Condition extends DebugCommand {
      val name = "condition"
      val desc = "set a DFDL expression to stop at breakpoint"
      override lazy val short = "con"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
        DebugState.Continue
      }
    }

    object Delete extends DebugCommand {
      val name = "delete"
      val desc = "delete a breakpoint"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

    object Display extends DebugCommand {
      val name = "display"
      val desc = "show value of expression each time program stops"
      override lazy val short = "di"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
        if (args.length == 0) {
           throw new DebugException("expression required for display command")
        } else {
          DebuggerConfig.displays += new Display(DebuggerConfig.displayIndex, args)
          DebuggerConfig.displayIndex += 1
        }
        DebugState.Pause
      }
    }

    object Disable extends DebugCommand {
      val name = "disable"
      val desc = "disable breakpoints and displays"
      override lazy val short = "dis"
      override val subcommands = Seq(DisableBreakpoint, DisableDisplay)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

    object Enable extends DebugCommand {
      val name = "enable"
      val desc = "enable various debugger elements"
      override val subcommands = Seq(EnableBreakpoint, EnableDisplay)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
        DebugCommandBase.help(args)
        DebugState.Pause
      }
    }

    object Info extends DebugCommand {
      val name = "info"
      val desc = "display information"
      override val subcommands = Seq(InfoArrayIndex, InfoBitLimit, InfoBitPosition, InfoBreakpoints, InfoChildIndex, InfoData, InfoDescriminator, InfoDisplays, InfoGroupIndex, InfoInfoset, InfoOccursCount, InfoPath)
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          if (DebuggerConfig.breakpoints.size == 0) {
            debugPrintln("%s: no breakpoints set".format(name))
          } else {
            debugPrintln("%s:".format(name))
            DebuggerConfig.breakpoints.foreach { b => {
              val enabledStr = if (b.enabled) "" else "*"
              debugPrintln("%s%s: %s   %s".format(b.id, enabledStr, b.breakpoint, b.condition.getOrElse("")), "  ")
            }}
          }
          DebugState.Pause
        }
      }

      object InfoChildIndex extends DebugCommand {
        val name = "childIndex"
        override lazy val short = "ci"
        val desc = "display the current child index"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

      object InfoDescriminator extends DebugCommand {
        val name = "descriminator"
        override lazy val short = "de"
        val desc = "display whether or not a descriminator is set"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          debugPrintln("%s: %b".format(name, state.discriminator))
          DebugState.Pause
        }
      }

      object InfoDisplays extends DebugCommand {
        val name = "displays"
        override lazy val short = "di"
        val desc = "display the current 'display' expressions"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          if (DebuggerConfig.displays.size == 0) {
            debugPrintln("%s: no displays set".format(name))
          } else {
            debugPrintln("%s:".format(name))
            DebuggerConfig.displays.foreach { d => {
              val enabledStr = if (d.enabled) "" else "*"
              debugPrintln("%s%s: %s".format(d.id, enabledStr, d.cmd.mkString(" ")), "  ")
            }}
          }
          DebugState.Pause
        }
      }

      object InfoGroupIndex extends DebugCommand {
        val name = "groupIndex"
        override lazy val short = "gi"
        val desc = "display the current group index"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          val currentNode = state.infoset
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
          val xmlNoHidden = XMLUtils.removeHiddenElements(xmlNode)
          val xmlClean = XMLUtils.removeAttributes(xmlNoHidden(0), Seq(NS(XMLUtils.INT_NS)))
          val wrap = if (DebuggerConfig.wrapLength <= 0) Int.MaxValue else DebuggerConfig.wrapLength
          val pp = new scala.xml.PrettyPrinter(wrap, 2)
          debugPrintln("%s:".format(name))
          val xml = pp.format(xmlClean)
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
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          if (state.occursCount != -1) {
            debugPrintln("%s: %d".format(name, state.occursCount))
          } else {
            debugPrintln("%s: occurs count not set".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoPath extends DebugCommand {
        val name = "path"
        val desc = "display the current schema component designator/path"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
          debugPrintln("%s: %s".format(name, parser.context.path))
          DebugState.Pause
        }
      }
    }

    object Set extends DebugCommand {
      val name = "set"
      val desc = "modify debugger configuration"
      override val subcommands = Seq(SetDataLength, SetInfosetLines, SetRemoveHidden, SetWrapLength)
      override lazy val short = "set"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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

      object SetDataLength extends DebugCommand {
        val name = "dataLength"
        val desc = "set the maximum number of bytes of the data to display"
        override lazy val short = "dl"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        override lazy val short = "tl"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        override lazy val short = "rh"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
        override lazy val short = "wl"
        def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
        DebugState.Step
      }
    }

    object UnDisplay extends DebugCommand {
      val name = "undisplay"
      val desc = "remove display"
      def apply(args: Seq[String], prestate: PState, state: PState, parser: Parser): DebugState.DebugState = {
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
