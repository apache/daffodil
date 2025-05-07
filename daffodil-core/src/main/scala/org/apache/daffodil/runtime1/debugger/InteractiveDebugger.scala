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

package org.apache.daffodil.runtime1.debugger

import java.io.File
import scala.collection.immutable.ArraySeq

import org.apache.daffodil.api.InfosetElement
import org.apache.daffodil.api.debugger.{
  InteractiveDebuggerRunner => JInteractiveDebuggerRunner
}
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.oolag.ErrorsNotYetRecorded
import org.apache.daffodil.lib.oolag.OOLAG._
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.util.DPathUtil
import org.apache.daffodil.lib.util.Enum
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.BasicComponent
import org.apache.daffodil.runtime1.dpath.ExpressionEvaluationException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dsom.ExpressionCompilerClass
import org.apache.daffodil.runtime1.dsom.RelativePathPastRootError
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors._
import org.apache.daffodil.runtime1.processors.parsers._
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UStateForSuspension
import org.apache.daffodil.runtime1.processors.unparsers.Unparser

abstract class InteractiveDebuggerRunner extends JInteractiveDebuggerRunner {
  def init(id: InteractiveDebugger): Unit
  def getCommand: String
  def lineOutput(line: String): Unit
  def fini(): Unit
}

case class DebuggerExitException() extends UnsuppressableException("Debugger exit")

class InteractiveDebugger(
  runner: JInteractiveDebuggerRunner,
  eCompilers: ExpressionCompilerClass
) extends Debugger {

  object DebugState extends Enum {
    sealed abstract trait Type extends EnumValueType
    case object Continue extends Type
    case object Step extends Type
    case object Pause extends Type
    case object Trace extends Type
  }

  case class DebugException(str: String, cause: Throwable)
    extends java.lang.Exception(str, cause) {
    override def toString = "Debugger error: " + Misc.getSomeMessage(this).get
    def this(str: String) = this(str, null)
  }

  trait Disablable {
    var enabled = true
    def disable() = { enabled = false }
    def enable() = { enabled = true }
  }

  case class Breakpoint(id: Int, breakpoint: String) extends Disablable {
    var condition: Option[String] = None
  }

  case class Display(id: Int, cmd: Seq[String]) extends Disablable

  object DebuggerConfig {
    /* the max number of lines to tail the infoset, infosetLines <= 0 means print
     * everything */
    var infosetLines: Int = -1

    /* the number of parent elements to include when displaying the infoset. -1
     * means show all parents. 0 or more means show 0 or more parent elements. */
    var infosetParents: Int = -1

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

    /* list of info commands to exclude when running 'info diff' */
    var diffExcludes: Seq[String] = Seq.empty

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
    var representation: Representation = Representation.Text
  }

  var debugState: DebugState.Type = DebugState.Pause

  var previousProcessorState: StateForDebugger = _

  override def init(state: PState, parser: Parser): Unit = {
    runner.init(this)
    previousProcessorState = state.copyStateForDebugger
  }

  override def init(state: UState, unparser: Unparser): Unit = {
    runner.init(this)
    previousProcessorState = state.copyStateForDebugger
  }

  override def fini(parser: Parser): Unit = {
    runner.fini()
  }

  override def fini(unparser: Unparser): Unit = {
    runner.fini()
  }

  def debugStep(
    state: ParseOrUnparseState,
    processor: Processor,
    ignoreBreakpoints: Boolean
  ): Unit = {
    // ignore debug steps called during suspensions, those must be handled differently
    if (state.isInstanceOf[UStateForSuspension]) {
      return
    }

    debugState = debugState match {
      case _ if ((state.processorStatus ne Success) && DebuggerConfig.breakOnFailure) =>
        DebugState.Pause
      case DebugState.Continue | DebugState.Trace if !ignoreBreakpoints => {
        findBreakpoint(state, processor) match {
          case Some(bp) => {
            debugPrintln(
              "breakpoint %s: %s   %s".format(bp.id, bp.breakpoint, bp.condition.getOrElse(""))
            )
            DebugState.Pause
          }
          case None => debugState
        }
      }
      case DebugState.Step => DebugState.Pause
      case _ => debugState
    }

    if (debugState == DebugState.Pause || debugState == DebugState.Trace) {
      val dc = DebuggerConfig
      val rawDisplays = dc.displays
      val displays = rawDisplays.filter(_.enabled)
      displays.foreach { d =>
        runCommand(d.cmd, state, processor)
      }

      if (state.processorStatus ne Success) {
        debugPrintln("failure:")
        debugPrintln("%s".format(state.diagnostics.head.getMessage()), "  ")
      }

      if (debugState == DebugState.Trace) {
        debugPrintln(
          "----------------------------------------------------------------- " + DebuggerConfig.parseStep
        )
      }
    }

    DebuggerConfig.parseStep += 1

    while (debugState == DebugState.Pause) {
      val args = readCmd()
      debugState = runCommand(args, state, processor)
    }

    previousProcessorState = state.copyStateForDebugger
  }

  private def isInteresting(parser: Parser): Boolean = {
    val interesting = parser match {
      case _: ComplexTypeParser => false
      case _: SeqCompParser => false
      case _: RepeatingChildParser => false
      case _: ConvertTextCombinatorParser => false
      case _: CombinatorParser => false
      case _ => true
    }
    interesting
  }

  override def startElement(state: PState, parser: Parser): Unit = {
    debugStep(state, parser, false)
  }

  override def endElement(state: UState, unparser: Unparser): Unit = {
    debugStep(state, unparser, false)
  }

  override def before(before: PState, parser: Parser): Unit = {}

  override def after(state: PState, parser: Parser): Unit = {
    if (isInteresting(parser)) {
      debugStep(state, parser, DebuggerConfig.breakOnlyOnCreation)
    }
  }

  override def beforeRepetition(before: PState, processor: Parser): Unit = {}

  override def afterRepetition(after: PState, processor: Parser): Unit = {}

  private def isInteresting(unparser: Unparser): Boolean = {
    true
  }

  override def before(before: UState, unparser: Unparser): Unit = {}

  override def after(state: UState, unparser: Unparser): Unit = {
    if (isInteresting(unparser)) {
      debugStep(state, unparser, DebuggerConfig.breakOnlyOnCreation)
    }
  }

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
    ArraySeq.unsafeWrapArray(cmd.split(" ").filter(_ != ""))
  }

  private val debuggerQName = GlobalQName(Some("daf"), "debugger", XMLUtils.dafintURI)

  /**
   * Here the debugger depends on being able to evaluate expressions that might run into
   * problems like asking for data from elements that don't exist yet or have no values yet.
   *
   * There also can be compilation errors if the expressions aren't well formed or have type errors in them (such as
   * they don't return a boolean value).
   */
  private def evaluateBooleanExpression(
    expression: String,
    state: ParseOrUnparseState,
    processor: Processor
  ): Boolean = {
    val context = state.getContext()
    try {
      //
      // compile the expression
      //
      val compiledExpr =
        try {
          val hostForDiags = new DebuggerHost(state.tunable)
          val ce = eCompilers.JBoolean.compileExpression(
            debuggerQName,
            NodeInfo.Boolean,
            expression,
            context.dpathCompileInfo.namespaces,
            context.dpathCompileInfo.noPrefixNamespace,
            context.dpathCompileInfo,
            false,
            hostForDiags,
            context.dpathCompileInfo
          )
          val warnings = hostForDiags.getDiagnostics.filterNot(_.isError)
          warnings.foreach {
            debugPrintln(_)
          }
          ce
        } catch {
          //
          // These are compile-time errors for the expression compilation
          //
          case errs: ErrorsNotYetRecorded => {
            debugPrintln(errs)
            throw errs
          }
        }
      //
      // evaluate the expression, and catch ways it can fail just because this is the debugger and it
      // isn't necessarily evaluating the expression in sensible places.
      //
      // Note also that the debugger does not use Evaluatable around the compiled expression. This is because
      // Evaluatable is really designed to be called from parsers/unparsers.
      //
      try {
        compiledExpr.evaluate(state).booleanValue()
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case _: ExpressionEvaluationException | _: InfosetException | _: VariableException => {
          // ?? How do we discern for the user whether this is a problem with their expression or
          // the infoset is just not populated with the things the expression references yet?
          state.setSuccess()
          false
        }
        //
        // Most errors are coming back here as RSDE because that's what they get upconverted into.
        // Most expression problems are considered SDE.
        //
        case e: RuntimeSchemaDefinitionError => {
          state.suppressDiagnosticAndSucceed(e)
          false
        }
      }
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Throwable => {
        debugPrintln(
          "caught throwable " + Misc.getNameFromClass(e) + ": " + Misc.getSomeMessage(e).get
        )
        state.setSuccess()
        false
      }
    }
  }

  private def findBreakpoint(
    state: ParseOrUnparseState,
    processor: Processor
  ): Option[Breakpoint] = {
    val foundBreakpoint =
      DebuggerConfig.breakpoints
        .filter(_.enabled)
        .filter { bp =>
          //
          // Two syntaxes for breakpoints are accepted.
          // one is extended QNames e.g., foo, or pre:foo, or {uri}foo
          // the other is schema component paths like foo::bar::baz
          //
          val tryBPQName = QName.refQNameFromExtendedSyntax(bp.breakpoint)
          if (tryBPQName.isFailure) {
            //
            // Breakpoint specified by path syntax
            //
            bp.breakpoint == processor.context.path
          } else {
            //
            // must be the extended QName case.
            //
            processor.context match {
              case erd: ElementRuntimeData => {
                val elemQName = erd.namedQName
                val bpqnx = tryBPQName.get
                //
                // If the user provided the {uri}foo style syntax
                // we just need the namespace and name to match.
                //
                if (
                  bpqnx.local == elemQName.local &&
                  bpqnx.namespace == elemQName.namespace
                ) {
                  true
                } else {
                  //
                  // usage must have been just a QName e.g., foo:bar
                  // for the breakpoint, or mostlikely, just a local name bar.
                  //
                  val bpQNameString = bpqnx.toQNameString
                  val bpqn = processor.context.resolveQName(bpQNameString)
                  val isMatch = bpqn.toStepQName.matches(elemQName)
                  if (isMatch)
                    true
                  else {
                    //
                    // finally, if the bp was just specified as a local name
                    // then ok so long as the local name part matches.
                    //
                    // TODO: it would be good to know if this bp name is ambiguous
                    // In this case it will match ANY element having that local
                    // name. But if you want to be more selective of just the
                    // specific element in a specific namespace then you can use
                    // the extended QName syntax, or just a prefix on it.
                    val isLocalMatch = bpqnx.local == elemQName.local
                    isLocalMatch
                  }
                }
              }
              case _ => false
            }
          }
        }
        .find { bp =>
          bp.condition match {
            case Some(expression) => evaluateBooleanExpression(expression, state, processor)
            case None => true
          }
        }
    foundBreakpoint
  }

  private def runCommand(
    cmd: Seq[String],
    state: ParseOrUnparseState,
    processor: Processor
  ): DebugState.Type = {
    try {
      DebugCommandBase(cmd, state, processor)
    } catch {
      case e: DebugException => {
        debugPrintln(e)
        DebugState.Pause
      }
    }
  }

  private def debugPrintln(obj: Any = "", prefix: String = ""): Unit = {
    obj.toString.split("\n").foreach { line =>
      {
        val out = "%s%s".format(prefix, line)
        runner.lineOutput(out)
      }
    }
  }

  private def infosetToString(ie: InfosetElement): String = {
    val bos = new java.io.ByteArrayOutputStream()
    val xml = new XMLTextInfosetOutputter(bos, pretty = true, minimal = true)
    val iw = InfosetWalker(
      ie.asInstanceOf[DIElement],
      xml,
      walkHidden = !DebuggerConfig.removeHidden,
      ignoreBlocks = true,
      releaseUnneededInfoset = false
    )
    iw.walk(lastWalk = true)
    bos.toString("UTF-8")
  }

  private def debugPrettyPrintXML(ie: InfosetElement): Unit = {
    val infosetString = infosetToString(ie)
    debugPrintln(infosetString)
  }

  /**********************************/
  /**          Commands            **/
  /**********************************/

  abstract class DebugCommand {
    def name: String
    lazy val short: String = name(0).toString
    val desc: String
    val longDesc: String
    val subcommands: Seq[DebugCommand] = Seq()
    val hidden = false

    def apply(
      args: Seq[String],
      state: ParseOrUnparseState,
      processor: Processor
    ): DebugState.Type = {
      validate(args)
      act(args, state, processor)
    }

    def validate(args: Seq[String]): Unit

    def act(
      args: Seq[String],
      state: ParseOrUnparseState,
      processor: Processor
    ): DebugState.Type

    def matches(str: String): Boolean = (str == name || str == short)

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
            prefixes.zip(descLines).foreach { case (p, d) =>
              debugPrintln(formatString.format(p, d))
            }
          })
        }
        case _ => {
          val subcmd = args.head
          val subcmdArgs = args.tail
          subcommands.find(_.matches(subcmd)) match {
            case Some(cmd) => cmd.help(subcmdArgs)
            case None => throw new DebugException("unknown command: %s".format(subcmd))
          }
        }
      }
    }

    // This ensures that there are no naming conflicts (e.g. short form names
    // conflict).
    def checkNameConflicts: Unit = {
      val allNames = subcommands.flatMap { cmd =>
        Seq(cmd.name, cmd.short).distinct
      }
      val duplicates = allNames.groupBy(identity).collect {
        case (elem, occurs) if occurs.size > 1 => elem
      }
      Assert.invariant(
        duplicates.size == 0,
        s"""Duplicate debug commands found in '$name' command: ${duplicates.mkString(", ")}"""
      )
      subcommands.foreach(_.checkNameConflicts)
    }
  }

  DebugCommandBase.checkNameConflicts

  trait DebugCommandValidateSubcommands { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.size == 0) {
        throw new DebugException("no command specified")
      }
      val subcmd = args.head
      val subcmdArgs = args.tail
      subcommands.find(_.matches(subcmd)) match {
        case Some(c) => c.validate(subcmdArgs)
        case None => {
          throw new DebugException("undefined command: %s".format(subcmd))
        }
      }
    }
  }

  trait DebugCommandValidateZeroArgs { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.length != 0) {
        throw new DebugException("%s command requires zero arguments".format(name))
      }
    }
  }

  trait DebugCommandValidateSingleArg { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.length != 1) {
        throw new DebugException("%s command requires a single argument".format(name))
      }
    }
  }

  trait DebugCommandValidateBoolean { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.size != 1) {
        throw new DebugException("%s command requires a single argument".format(name))
      } else {
        val state = args.head
        if (state != "true" && state != "1" && state != "false" && state != "0") {
          throw new DebugException("argument must be true/false or 1/0")
        }
      }
    }
  }

  trait DebugCommandValidateInt { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.size != 1) {
        throw new DebugException("%s command requires a single argument".format(name))
      } else {
        try {
          args.head.toInt
        } catch {
          case _: NumberFormatException =>
            throw new DebugException("integer argument is required")
        }
      }
    }
  }

  trait DebugCommandValidateOptionalArg { self: DebugCommand =>
    override def validate(args: Seq[String]): Unit = {
      if (args.size > 1) {
        throw new DebugException("%s command zero or one arguments".format(name))
      }
    }
  }

  object DebugCommandBase extends DebugCommand with DebugCommandValidateSubcommands {
    val name = ""
    val desc = ""
    val longDesc = ""
    override lazy val short = ""
    override val subcommands = Seq(
      Break,
      Clear,
      Complete,
      Condition,
      Continue,
      Delete,
      Disable,
      Display,
      Enable,
      Eval,
      Help,
      History,
      Info,
      Quit,
      Set,
      Step,
      Trace
    )

    def act(
      args: Seq[String],
      state: ParseOrUnparseState,
      processor: Processor
    ): DebugState.Type = {
      val subcmd = args.head
      val subcmdArgs = args.tail
      val subcmdActor = subcommands.find(_.matches(subcmd)).get
      val newState = subcmdActor.act(subcmdArgs, state, processor)
      newState
    }

    object Break extends DebugCommand with DebugCommandValidateSingleArg {
      val name = "break"
      val desc = "create a breakpoint"
      val longDesc = """|Usage: b[reak] <element_id>
                        |
                        |Create a breakpoint, causing the debugger to stop when the element
                        |with the <element_id> name is created.
                        |
                        |Example: break foo""".stripMargin

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val bp = new Breakpoint(DebuggerConfig.breakpointIndex, args.head)
        DebuggerConfig.breakpoints += bp
        debugPrintln("%s: %s".format(bp.id, bp.breakpoint))
        DebuggerConfig.breakpointIndex += 1
        DebugState.Pause
      }
    }

    object Clear extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "clear"
      val desc = "clear the screen"
      val longDesc = """|Usage: cl[ear]
                        |
                        |Clear the screen.""".stripMargin
      override lazy val short = "cl"

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        print(27: Char)
        print('[')
        print("2J")
        print(27: Char)
        print('[')
        print("1;1H")

        DebugState.Pause
      }
    }

    object Complete extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "complete"
      val desc = "disable all debugger actions and continue"
      val longDesc = """|Usage: comp[lete]
                        |
                        |Continue parsing the input data until parsing is complete. All
                        |breakpoints are ignored.""".stripMargin
      override lazy val short = "comp"

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebuggerConfig.breakpoints.foreach(_.disable())
        DebuggerConfig.displays.foreach(_.disable())
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
                        |evaluates to true. If the result of the DFDL expression is not
                        |a boolean value, it is treated as false.
                        |
                        |Example: condition 1 dfdl:occursIndex() eq 3""".stripMargin
      override lazy val short = "cond"

      override def validate(args: Seq[String]): Unit = {
        if (args.length < 2) {
          throw new DebugException(
            "condition command requires a breakpoint id and a DFDL expression"
          )
        }

        val idArg = args.head
        val id =
          try {
            idArg.toInt
          } catch {
            case _: NumberFormatException =>
              throw new DebugException("integer argument required")
          }

        DebuggerConfig.breakpoints.find(_.id == id).getOrElse {
          throw new DebugException("breakpoint %d not found".format(id))
        }
      }

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val id = args.head.toInt
        val expression = args.tail.mkString(" ")
        val expressionWithBraces =
          if (!DPathUtil.isExpression(expression)) "{ " + expression + " }"
          else expression
        val b = DebuggerConfig.breakpoints.find(_.id == id).get
        b.condition = Some(expressionWithBraces)
        debugPrintln("%s: %s   %s".format(b.id, b.breakpoint, expressionWithBraces))
        DebugState.Pause
      }
    }

    object Continue extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "continue"
      val desc = "continue parsing until a breakpoint is found"
      val longDesc = """|Usage: c[ontinue]
                        |
                        |Continue parsing the input data until a breakpoint is encountered. At
                        |which point, pause parsing and display a debugger console to the user.""".stripMargin

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebugState.Continue
      }
    }

    object Delete extends DebugCommand with DebugCommandValidateSubcommands {
      val name = "delete"
      val desc = "delete breakpoints and displays"
      val longDesc = """|Usage: d[elete] <type> <id>
                        |
                        |Remove a breakpoint or display.
                        |
                        |Example: delete breakpoint 1
                        |         delete display 1""".stripMargin
      override val subcommands = Seq(DeleteBreakpoint, DeleteDisplay)

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val subcmd = args.head
        val subcmdArgs = args.tail
        subcommands.find(_.matches(subcmd)).get.act(subcmdArgs, state, processor)
        DebugState.Pause
      }

      object DeleteBreakpoint extends DebugCommand with DebugCommandValidateInt {
        val name = "breakpoint"
        val desc = "delete a breakpoint"
        val longDesc = """|Usage: d[elete] b[reakpoint] <breakpoint_id>
                          |
                          |Remove a breakpoint created using the 'breakpoint' command.
                          |
                          |Example: delete breakpoint 1""".stripMargin

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => DebuggerConfig.breakpoints -= b
            case None => throw new DebugException("breakpoint %d not found".format(id))
          }
          DebugState.Pause
        }
      }

      object DeleteDisplay extends DebugCommand with DebugCommandValidateInt {
        val name = "display"
        val desc = "delete a display"
        val longDesc = """|Usage: d[elete] di[splay] <display_id>
                          |
                          |Remove a display created using the 'display' command.
                          |
                          |Example: delete display 1""".stripMargin
        override lazy val short = "di"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.displays.find(d => d.id == id) match {
            case Some(d) => DebuggerConfig.displays -= d
            case None => throw new DebugException("display %d not found".format(id))
          }
          DebugState.Pause
        }
      }
    }

    object Disable extends DebugCommand with DebugCommandValidateSubcommands {
      val name = "disable"
      val desc = "disable breakpoints and displays"
      val longDesc = """|Usage: dis[able] <type> <id>
                        |
                        |Disable a breakpoint or display.
                        |
                        |Example: disable breakpoint 1
                        |         disable display 1""".stripMargin
      override lazy val short = "dis"
      override val subcommands = Seq(DisableBreakpoint, DisableDisplay)

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val subcmd = args.head
        val subcmdArgs = args.tail
        subcommands.find(_.matches(subcmd)).get.act(subcmdArgs, state, processor)
        DebugState.Pause
      }

      object DisableBreakpoint extends DebugCommand with DebugCommandValidateInt {
        val name = "breakpoint"
        val desc = "disable a breakpoint"
        val longDesc = """|Usage: dis[able] b[reakpoint] <breakpoint_id>
                          |
                          |Disable a breakpoint with the specified id. This causes the breakpoint
                          |to be skipped during debugging.
                          |
                          |Example: disable breakpoint 1""".stripMargin

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => b.disable()
            case None => throw new DebugException("%d is not a valid breakpoint id".format(id))
          }
          DebugState.Pause
        }
      }

      object DisableDisplay extends DebugCommand with DebugCommandValidateInt {
        val name = "display"
        val desc = "disable a display"
        val longDesc = """|Usage: d[isable] di[splay] <display_id>
                          |
                          |Disable a display with the specified id. This causes the display command
                          |to be skipped during debugging.
                          |
                          |Example: disable display 1""".stripMargin
        override lazy val short = "di"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.displays.find(_.id == id) match {
            case Some(d) => d.disable()
            case None => throw new DebugException("%d is not a valid display id".format(id))
          }
          DebugState.Pause
        }
      }
    }

    object Display extends DebugCommand with DebugCommandValidateSubcommands {
      val name = "display"
      val desc = "show value of expression each time program stops"
      val longDesc = """|Usage: di[splay] <debugger_command>
                        |
                        |Execute a debugger command (limited to eval, info, and clear) every time a
                        |there is a pause in the debugger.
                        |
                        |Example: display info infoset""".stripMargin
      override lazy val short = "di"
      override val subcommands = Seq(Eval, Info, Clear)

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebuggerConfig.displays += new Display(DebuggerConfig.displayIndex, args)
        DebuggerConfig.displayIndex += 1
        DebugState.Pause
      }
    }

    object Enable extends DebugCommand with DebugCommandValidateSubcommands {
      val name = "enable"
      val desc = "enable breakpoints and displays"
      val longDesc = """|Usage: e[nable] <type> <id>
                        |
                        |Enable a breakpoint or display.
                        |
                        |Example: enable breakpoint 1
                        |         enable display 1""".stripMargin
      override val subcommands = Seq(EnableBreakpoint, EnableDisplay)

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val subcmd = args.head
        val subcmdArgs = args.tail
        subcommands.find(_.matches(subcmd)).get.act(subcmdArgs, state, processor)
        DebugState.Pause
      }

      object EnableBreakpoint extends DebugCommand with DebugCommandValidateInt {
        val name = "breakpoint"
        val desc = "enable a breakpoint"
        val longDesc = """|Usage: e[nable] b[reakpoint] <breakpoint_id>
                          |
                          |Enable a breakpoint with the specified id. This causes the breakpoint
                          |to be evaluated during debugging.
                          |
                          |Example: enable breakpoint 1""".stripMargin

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.breakpoints.find(_.id == id) match {
            case Some(b) => b.enable()
            case None => throw new DebugException("%d is not a valid breakpoint id".format(id))
          }
          DebugState.Pause
        }
      }

      object EnableDisplay extends DebugCommand with DebugCommandValidateInt {
        val name = "display"
        val desc = "enable a display"
        val longDesc = """|Usage: e[nable] di[splay] <display_id>
                          |
                          |Enable a display with the specified id. This causes the display command
                          |to be run during debugging.
                          |
                          |Example: enable display 1""".stripMargin

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val id = args.head.toInt
          DebuggerConfig.displays.find(_.id == id) match {
            case Some(d) => d.enable()
            case None => throw new DebugException("%d is not a valid display id".format(id))
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

      override def validate(args: Seq[String]): Unit = {
        if (args.size == 0) {
          throw new DebugException("eval requires a DFDL expression")
        }
      }

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val expressionList = args
        val expression = expressionList.mkString(" ")

        if (!state.hasInfoset) {
          debugPrintln("eval: There is no infoset currently.")
          return DebugState.Pause
        }
        val element = state.infoset
        // this adjustment is so that automatic display of ".." doesn't fail
        // for the root element.
        val adjustedExpression =
          if ((element.parent eq null) && (expression == "..")) "."
          else expression
        val context = state.getContext()
        val namespaces = context.dpathCompileInfo.namespaces
        val noPrefixNamespace = context.dpathCompileInfo.noPrefixNamespace
        val expressionWithBraces =
          if (!DPathUtil.isExpression(adjustedExpression)) "{ " + adjustedExpression + " }"
          else adjustedExpression
        val isEvaluatedAbove = false
        try {
          val hostForDiags = new DebuggerHost(state.tunable)
          val compiledExpression = eCompilers.AnyRef.compileExpression(
            debuggerQName,
            NodeInfo.AnyType,
            expressionWithBraces,
            namespaces,
            noPrefixNamespace,
            context.dpathCompileInfo,
            isEvaluatedAbove,
            hostForDiags,
            context.dpathElementCompileInfo
          )
          val res = compiledExpression.evaluate(state)
          val warnings = hostForDiags.getDiagnostics.filterNot(_.isError)
          warnings.foreach {
            debugPrintln(_)
          }
          res match {
            case ie: InfosetElement => debugPrettyPrintXML(ie)
            case nodeSeq: Seq[Any] =>
              nodeSeq.foreach { a =>
                a match {
                  case ie: InfosetElement => debugPrettyPrintXML(ie)
                  case _ => debugPrintln(a)
                }
              }
            case _ => debugPrintln(res)
          }
        } catch {
          case e: ErrorsNotYetRecorded => {
            val diags = e.diags
            val newDiags = diags.flatMap { d =>
              d match {
                case rel: RelativePathPastRootError => Nil
                case _ => List(d)
              }
            }
            if (!newDiags.isEmpty) {
              val ex = new ErrorsNotYetRecorded(newDiags)
              throw new DebugException(
                "expression evaluation failed: %s".format(Misc.getSomeMessage(ex).get)
              )
            }
          }
          case s: scala.util.control.ControlThrowable => throw s
          //
          // If we eval(.) on a node that has no value, we get a RSDE thrown.
          //
          // Users (such as tests in daffodil's cli module) can set up a 'display eval (.)' and then
          // single steps until they start parsing an element which has no value.
          // That will throw this RSDE. If we recognize this situation, we
          // display the empty element.
          //
          case r: RuntimeSchemaDefinitionError if r.getCause() ne null =>
            r.getCause() match {
              case nd: InfosetNoDataExceptionBase => {
                //
                // Displays the empty element since it has no value.
                //
                debugPrettyPrintXML(nd.diElement)
                state.suppressDiagnosticAndSucceed(r)
              }
              case _ => throw r
            }
          case e: Throwable => {
            val ex = e // just so we can see it in the debugger.
            throw new DebugException(
              "expression evaluation failed: %s".format(Misc.getSomeMessage(ex).get)
            )
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
      override val subcommands = Seq(
        Break,
        Clear,
        Complete,
        Condition,
        Continue,
        Delete,
        Disable,
        Display,
        Enable,
        Eval,
        History,
        Info,
        Quit,
        Set,
        Step,
        Trace
      )

      override def validate(args: Seq[String]): Unit = {
        // no validation
      }

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebugCommandBase.help(args)
        DebugState.Pause
      }
    }

    object History extends DebugCommand with DebugCommandValidateOptionalArg {
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

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        args.size match {
          case 0 => {
            debugPrintln("%s:".format(name))
            DebuggerConfig.history.zipWithIndex.foreach { case (cmd, index) =>
              debugPrintln("%d: %s".format(index, cmd), "  ")
            }

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
              case s: scala.util.control.ControlThrowable => throw s
              case u: UnsuppressableException => throw u
              case e: Throwable =>
                throw new DebugException("failed to write history file: " + e.getMessage())
            }
          }
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
                        |multiple times to display multiple pieces of information. <items>
                        |that are not recognized as info commands are assumed to be arguments
                        |to the previous <item>
                        |
                        |Example: info data infoset""".stripMargin
      override val subcommands =
        Seq(
          InfoBitLimit,
          InfoBitPosition,
          InfoBreakpoints,
          InfoChildIndex,
          InfoData,
          InfoDelimiterStack,
          InfoDiff,
          InfoDisplays,
          InfoFoundDelimiter,
          InfoFoundField,
          InfoGroupIndex,
          InfoHidden,
          InfoInfoset,
          InfoOccursIndex,
          InfoPath,
          InfoParser,
          InfoPointsOfUncertainty,
          InfoSuspensions,
          InfoUnparser,
          InfoVariables
        )

      /**
      * The info command allows printing multiple different kinds of
      * information at once. For example "info foo bar" is equivalent to
      * running the two commands "info foo" and "info bar". However, some info
      * commands might take one or more parameters. In this case, we allow
      * include the parameters after the subcommand, for example "info foo
      * fooParam bar". To determine where one subcommand end and another
      * subcommand begins, we assume unknown strings (e.g. fooParam) are parameters
      * to the previous known subcommand. This function constructs a sequence
      * that represents the different info commands and their parameters. For
      * example, the following command:
      *
      *   info foo bar barParam1 barParam2 baz
      *
      * Is parsed to the following:
      *
      *   Seq(
      *     Seq("foo"),
      *     Seq("bar", "barParam1", "barParam2"),
      *     Seq("baz"),
      *   )
      *
      * This sequence of sequences can then be used to determine how to execute
      * individual info commands and provide the appropriate arguments.
      */
      private def buildInfoCommands(args: Seq[String]): Seq[Seq[String]] = {
        val backwardsInfoCommands = args.foldLeft(Seq.empty[Seq[String]]) {
          case (infoCmds, arg) =>
            val cmd = subcommands.find(_.matches(arg))
            if (cmd.isDefined || infoCmds.isEmpty) {
              // Found a new info subcommand, or we don't have an info commands
              // yet. Create a new Seq to hold the subcommand + args and
              // prepend this Seq to our list of info subcommands. Note that if
              // this isn't actually an info subcommand, we'll detect that later
              // when we validate this list.
              val newCommand = Seq(arg)
              newCommand +: infoCmds
            } else {
              // Not a recognized info subcommand. Assume it is an arg to the
              // most recent command we've seen and prepend it to that list.
              val head :: tail = infoCmds
              (arg +: head) +: tail
            }
        }

        // We've built up a list of info commands with args, but the info
        // commands and the args are all reversed because we prepended
        // everything. So reverse that all to get the order correct
        backwardsInfoCommands.map { _.reverse }.reverse
      }

      override def validate(args: Seq[String]): Unit = {
        if (args.size == 0) {
          throw new DebugException("one or more commands are required")
        }
        val infocmds = buildInfoCommands(args)
        infocmds.foreach { cmds =>
          val cmd :: args = cmds
          subcommands.find(_.matches(cmd)) match {
            case Some(c) => c.validate(args)
            case None => throw new DebugException("undefined info command: %s".format(cmd))
          }
        }
      }

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val infocmds = buildInfoCommands(args)
        infocmds.foreach { cmds =>
          val cmd :: args = cmds
          val action = subcommands.find(_.matches(cmd)).get
          action.act(args, state, processor)
        }
        DebugState.Pause
      }

      trait InfoDiffable extends DebugCommand {

        /**
        * Outputs any differences between previousProcessorState and state for the mixed in debugger command
        *
        * Differences should be displayed via the debugPrintln command. Output
        * should include the command name and two space indentation (e.g. pass
        * in "  " as the second argument of debugPrintln).
        *
        * @return true if any differences were found and output, false otherwise
        */
        def diff(pre: StateForDebugger, post: StateForDebugger): Boolean
      }

      trait InfoSimpleValue[A] extends InfoDiffable { self: DebugCommand =>

        def getSomeValue(state: StateForDebugger): Option[A]

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val strValue = getSomeValue(state).map(_.toString).getOrElse("(no value)")
          debugPrintln("%s: %s".format(self.name, strValue))
          DebugState.Pause
        }

        def diff(pre: StateForDebugger, post: StateForDebugger): Boolean = {
          val valPre = getSomeValue(pre)
          val valPost = getSomeValue(post)
          if (valPre != valPost) {
            val strPre = valPre.map(_.toString).getOrElse("(no value)")
            val strPost = valPost.map(_.toString).getOrElse("(no value)")
            debugPrintln("%s: %s -> %s".format(self.name, strPre, strPost), "  ")
            true
          } else {
            false
          }
        }
      }

      trait InfoSeqValue[A] extends InfoDiffable { self: DebugCommand =>

        def getSeqValue(state: StateForDebugger): Seq[A]

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s:".format(self.name))
          getSeqValue(state).foreach { value =>
            debugPrintln("%s".format(value.toString), "  ")
          }
          DebugState.Pause
        }

        def diff(pre: StateForDebugger, post: StateForDebugger): Boolean = {
          val preSeq = getSeqValue(pre)
          val postSeq = getSeqValue(post)
          val removed = preSeq.diff(postSeq)
          val added = postSeq.diff(preSeq)
          val hasDiff = removed.nonEmpty || added.nonEmpty
          if (hasDiff) {
            debugPrintln("%s:".format(self.name), "  ")
            removed.foreach { v =>
              debugPrintln("- %s".format(v.toString), "    ")
            }
            added.foreach { v =>
              debugPrintln("+ %s".format(v.toString), "    ")
            }
          }
          hasDiff
        }
      }

      object InfoBitLimit
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Long] {
        val name = "bitLimit"
        override lazy val short = "bl"
        val desc = "display the current bit limit"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[Long] = {
          if (state.bitLimit0b.isDefined) Some(state.bitLimit0b.get) else None
        }
      }

      object InfoBitPosition
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Long] {
        val name = "bitPosition"
        override lazy val short = "bp"
        val desc = "display the current bit position"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[Long] = {
          if (state.bitPos0b != -1) Some(state.bitPos0b) else None
        }
      }

      object InfoBreakpoints extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "breakpoints"
        val desc = "display the current breakpoints"
        val longDesc = desc
        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          if (DebuggerConfig.breakpoints.size == 0) {
            debugPrintln("%s: no breakpoints set".format(name))
          } else {
            debugPrintln("%s:".format(name))
            DebuggerConfig.breakpoints.foreach { b =>
              {
                val enabledStr = if (b.enabled) "" else "*"
                debugPrintln(
                  "%s%s: %s   %s"
                    .format(b.id, enabledStr, b.breakpoint, b.condition.getOrElse("")),
                  "  "
                )
              }
            }
          }
          DebugState.Pause
        }
      }

      object InfoChildIndex
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Long] {
        val name = "childIndex"
        override lazy val short = "ci"
        val desc = "display the current child index"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[Long] = {
          if (state.childPos != -1) Some(state.childPos) else None
        }
      }

      object InfoData extends DebugCommand with DebugCommandValidateOptionalArg {
        val name = "data"
        val desc = "display the input/output data"
        val longDesc = desc

        def printData(
          rep: Option[Representation],
          l: Int,
          prestate: StateForDebugger,
          state: ParseOrUnparseState,
          processor: Processor
        ): Unit = {
          val dataLoc = prestate.currentLocation.asInstanceOf[DataLoc]
          val lines = dataLoc.dump(rep, prestate.currentLocation, state)
          debugPrintln(lines, "  ")
        }

        override def validate(args: Seq[String]): Unit = {
          super.validate(args)
          args.headOption.map(_.toLowerCase) match {
            case None =>
            case Some("text") =>
            case Some("binary") =>
            case _ =>
              throw new DebugException(
                "unknown data representation: %s. Must be one of 'text' or 'binary'".format(
                  args(0)
                )
              )
          }
        }

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s:".format(name))
          val rep = if (args.size > 0) {
            args(0).toLowerCase match {
              case "text" => Some(Representation.Text)
              case "binary" => Some(Representation.Binary)
            }
          } else {
            if (state.hasInfoset) {
              state.infoset match {
                case e: DIElement => Some(e.erd.impliedRepresentation)
              }
            } else {
              None
            }
          }

          val len = if (args.size > 1) {
            try {
              args(1).toInt
            } catch {
              case _: NumberFormatException =>
                throw new DebugException("data length must be an integer")
            }
          } else {
            DebuggerConfig.dataLength
          }

          printData(rep, len, previousProcessorState, state, processor)
          DebugState.Pause
        }
      }

      object InfoDelimiterStack extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "delimiterStack"
        val desc = "display the delimiter stack"
        val longDesc = desc
        override lazy val short = "ds"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s:".format(name))

          state match {
            case pstate: PState => {
              var i = 0
              while (i < pstate.mpstate.delimiters.length) {
                val typeString =
                  if (i < pstate.mpstate.delimitersLocalIndexStack.top) "remote:" else "local: "
                val delim = pstate.mpstate.delimiters(i)
                debugPrintln(
                  "%s %s (%s)"
                    .format(typeString, delim.lookingFor, delim.delimType.toString.toLowerCase),
                  "  "
                )
                i += 1
              }
            }
            case ustate: UState => {
              // TODO
            }
            case _ => Assert.impossibleCase()
          }

          DebugState.Pause
        }
      }

      object InfoDiff extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "diff"
        override lazy val short = "diff"
        val desc = "display differences since the previous pause in the debugger"
        val longDesc = desc

        lazy val infoDiffables = Info.subcommands
          .filter(_.isInstanceOf[InfoDiffable])
          .map(_.asInstanceOf[InfoDiffable])

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s:".format(name))
          val foundDiff = infoDiffables.foldLeft(false) { case (prevCmdsFoundDiff, curCmd) =>
            val curCmdFoundDiff =
              if (DebuggerConfig.diffExcludes.exists(de => curCmd.matches(de))) {
                false // skip current command since it's excluded
              } else {
                curCmd.diff(previousProcessorState, state)
              }
            curCmdFoundDiff || prevCmdsFoundDiff
          }
          if (!foundDiff) {
            debugPrintln("(no differences)", "  ")
          }

          DebugState.Pause
        }
      }

      object InfoDisplays extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "displays"
        override lazy val short = "di"
        val desc = "display the current 'display' expressions"
        val longDesc = desc
        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
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

      object InfoFoundDelimiter
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[String] {
        val name = "foundDelimiter"
        override lazy val short = "fd"
        val desc = "display the current found delimiter"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[String] = {
          if (state.delimitedParseResult.isDefined) {
            val dpr = state.delimitedParseResult.get
            val value = Misc.remapStringToVisibleGlyphs(dpr.matchedDelimiterValue.get)
            Some(value)
          } else {
            None
          }
        }
      }

      object InfoFoundField
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[String] {
        val name = "foundField"
        override lazy val short = "ff"
        val desc = "display the current found field when delimiter scanning"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[String] = {
          if (state.delimitedParseResult.isDefined) {
            val dpr = state.delimitedParseResult.get
            val value = Misc.remapStringToVisibleGlyphs(dpr.field.get)
            Some(value)
          } else {
            None
          }
        }
      }

      object InfoGroupIndex
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Long] {
        val name = "groupIndex"
        override lazy val short = "gi"
        val desc = "display the current group index"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[Long] = {
          if (state.groupPos != -1) Some(state.groupPos) else None
        }
      }

      object InfoHidden
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Boolean] {
        val name = "hidden"
        override lazy val short = "h"
        val desc = "display whether or not we're within the nesting context of a hidden group"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Some[Boolean] = {
          Some(state.withinHiddenNest)
        }
      }

      object InfoInfoset extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "infoset"
        val desc = "display the current infoset"
        val longDesc = desc

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s:".format(name))

          if (state.hasInfoset) {
            var parentSteps =
              if (DebuggerConfig.infosetParents < 0) Int.MaxValue
              else DebuggerConfig.infosetParents

            var node = state.infoset
            while (parentSteps > 0 && node.diParent != null) {
              node = node.diParent
              parentSteps -= 1
            }

            node match {
              case d: DIDocument if d.numChildren == 0 => {
                debugPrintln("No Infoset", "  ")
              }
              case _ => {
                val infosetString = infosetToString(node)
                val lines = infosetString.split("\r?\n")

                val dropCount =
                  if (DebuggerConfig.infosetLines < 0) 0
                  else Math.max(0, lines.size - DebuggerConfig.infosetLines)
                if (dropCount > 0) {
                  debugPrintln("...", "  ")
                }
                val linesToShow = lines.drop(dropCount)
                linesToShow.foreach { l => debugPrintln(l, "  ") }
              }
            }
          } else {
            debugPrintln("No Infoset", "  ")
          }

          DebugState.Pause
        }
      }

      object InfoOccursIndex
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSimpleValue[Long] {
        val name = "occursIndex"
        override lazy val short = "oi"
        val desc = "display the current array limit"
        val longDesc = desc

        def getSomeValue(state: StateForDebugger): Option[Long] = {
          if (state.occursPos != -1) Some(state.occursPos) else None
        }
      }

      object InfoPath extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "path"
        override lazy val short = "path"
        val desc = "display the current schema component designator/path"
        val longDesc = desc
        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          debugPrintln("%s: %s".format(name, processor.context.path))
          DebugState.Pause
        }
      }

      abstract class InfoProcessorBase extends DebugCommand with DebugCommandValidateZeroArgs {
        val desc = "display the current Daffodil " + this.name
        val longDesc = desc
        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          state match {
            case pstate: PState => {
              if (this.name == "parser") {
                debugPrintln("%s: %s".format(this.name, processor.toBriefXML(2)))
              } else {
                debugPrintln("unparser: not available")
              }
            }
            case ustate: UState => {
              if (this.name == "unparser") {
                debugPrintln("%s: %s".format(this.name, processor.toBriefXML(2)))
              } else {
                debugPrintln("parser: not available")
              }
            }
          }
          DebugState.Pause
        }
      }

      object InfoParser extends InfoProcessorBase {
        override lazy val name = "parser" // scala -xcheckinit reported this was uninitialized
      }

      object InfoPointsOfUncertainty extends DebugCommand with DebugCommandValidateZeroArgs {
        val name = "pointsOfUncertainty"
        override lazy val short = "pou"
        val desc = "display list of unresolved points of uncertainty"
        val longDesc = desc
        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          state match {
            case state: PState => {
              debugPrintln("%s:".format(name))
              val pous = state.pointsOfUncertainty.toList
              if (pous.isEmpty) {
                debugPrintln("  (none)")
              } else {
                pous.reverse.foreach { pou =>
                  debugPrintln("  %s".format(pou.toString))
                }
              }
            }
            case _ => debugPrintln("%s: info only available for parse steps".format(name))
          }
          DebugState.Pause
        }
      }

      object InfoSuspensions
        extends DebugCommand
        with DebugCommandValidateZeroArgs
        with InfoSeqValue[Suspension] {
        val name = "suspensions"
        override lazy val short = "sus"
        val desc = "display list of suspensions"
        val longDesc = desc

        def getSeqValue(state: StateForDebugger): Seq[Suspension] = state.suspensions
      }

      object InfoUnparser extends InfoProcessorBase {
        override lazy val name = "unparser"
      }

      object InfoVariables extends DebugCommand with InfoDiffable {
        val name = "variables"
        override lazy val short = "v"
        val desc = "display in-scope state of variables"
        val longDesc = """|Usage: v[ariables] [<name>...]
                          |
                          |Display the in-scope state of variables matching <name>'s. If no
                          |names are given, displays the in-scope state of all variabes.""".stripMargin

        override def validate(args: Seq[String]): Unit = {
          // no validation
        }

        def variableInstanceToDebugString(vinst: VariableInstance): String = {
          val state = vinst.state match {
            case VariableDefined => "default"
            case VariableRead => "read"
            case VariableSet => "set"
            case VariableUndefined => "undefined"
            case VariableBeingDefined => "being defined"
            case VariableInProcess => "in process"
          }

          if (vinst.value.isEmpty) "(%s)".format(state)
          else "%s (%s)".format(vinst.value.value, state)
        }

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val vmap = state.variableMapForDebugger
          val allQNames = vmap.qnames
          val qnamesToPrint =
            if (args.size == 0) allQNames
            else {
              allQNames.filter { qname =>
                args.contains(qname.local) || args.contains(qname.toPrettyString)
              }
            }

          debugPrintln("%s:".format(name))
          qnamesToPrint.sortBy { _.toPrettyString }.foreach { qname =>
            val instance = vmap.find(qname).get
            val debugVal = variableInstanceToDebugString(instance)
            debugPrintln("  %s: %s".format(qname.toPrettyString, debugVal))
          }

          DebugState.Pause
        }

        def diff(pre: StateForDebugger, post: StateForDebugger): Boolean = {
          val prevmap = pre.variableMapForDebugger
          prevmap.qnames.foldLeft(false) { case (foundDiff, qname) =>
            val pre_instance = prevmap.find(qname).get
            val pre_value = pre_instance.value
            val pre_state = pre_instance.state

            val cur_instance = post.variableMapForDebugger.find(qname).get
            val cur_value = cur_instance.value
            val cur_state = cur_instance.state

            if (pre_value != cur_value || pre_state != cur_state) {
              debugPrintln(
                "variable: %s: %s -> %s".format(
                  qname,
                  variableInstanceToDebugString(pre_instance),
                  variableInstanceToDebugString(cur_instance)
                ),
                "  "
              )
              foundDiff || true
            } else {
              foundDiff
            }
          }
        }
      }
    }

    object Quit extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "quit"
      val desc = "immediately abort all processing"
      val longDesc = """|Usage: q[uit]
                        |
                        |Immediately abort all processing.""".stripMargin
      override lazy val short = "q"
      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        throw new DebuggerExitException()
      }
    }

    object Set extends DebugCommand with DebugCommandValidateSubcommands {
      val name = "set"
      val desc = "modify debugger configuration"
      val longDesc = """|Usage: set <setting> <value>
                        |
                        |Change a debugger setting, the list of settings are below.
                        |
                        |Example: set breakOnlyOnCreation false
                        |         set dataLength 100""".stripMargin
      override val subcommands: Seq[DebugCommand] = Seq(
        SetBreakOnFailure,
        SetBreakOnlyOnCreation,
        SetDataLength,
        SetDiffExcludes,
        SetInfosetLines,
        SetInfosetParents,
        SetRemoveHidden,
        SetRepresentation,
        SetWrapLength
      )
      override lazy val short = "set"

      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        val subcmd = args.head
        val subcmdArgs = args.tail
        subcommands.find(_.matches(subcmd)).get.act(subcmdArgs, state, processor)
        DebugState.Pause
      }

      object SetBreakOnlyOnCreation extends DebugCommand with DebugCommandValidateBoolean {
        val name = "breakOnlyOnCreation"
        val desc =
          "whether or not breakpoints should occur only on element creation, or always (default: true)"
        val longDesc = """|Usage: set breakOnlyOnCreation|booc <value>
                          |
                          |Set whether or not breakpoints should only be evaluated on element creation.
                          |<value> must be either true/false or 1/0. If true, breakpoints only stop on
                          |element creation. If false, breakpoints stop anytime a parser interacts with
                          |an element. Defaults to true.
                          |
                          |Example: set breakOnlyOnCreation false""".stripMargin
        override lazy val short = "booc"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val state = args.head
          DebuggerConfig.breakOnlyOnCreation = if (state == "true" || state == "1") {
            true
          } else {
            false
          }
          DebugState.Pause
        }
      }

      object SetBreakOnFailure extends DebugCommand with DebugCommandValidateBoolean {
        val name = "breakOnFailure"
        val desc = "whether or not the debugger should break on failures (default: false)"
        val longDesc = """|Usage: set breakOnFailure|bof <value>
                          |
                          |Set whether or not the debugger should break on failures. If set to false
                          |the normal processing occurs. If set to true, any errors cause a break.
                          |Note that due to the backtracking behavior, not all failures are fatal.
                          |Defaults to false.
                          |
                          |Example: set breakOnFailure true""".stripMargin
        override lazy val short = "bof"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val state = args.head
          DebuggerConfig.breakOnFailure = if (state == "true" || state == "1") {
            true
          } else {
            false
          }
          DebugState.Pause
        }
      }

      object SetDataLength extends DebugCommand with DebugCommandValidateInt {
        val name = "dataLength"
        val desc =
          "set the maximum number of bytes of the data to display. If negative, display all input data (default: 70)"
        val longDesc = """|Usage: set dataLength|dl <value>
                          |
                          |Set the number of bytes to display when displaying input data. If
                          |negative, display all input data. This only affects the 'info data'
                          |command. Defaults to 70 bytes.
                          |
                          |Example: set dataLength 100
                          |         set dataLength -1""".stripMargin
        override lazy val short = "dl"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.dataLength = args.head.toInt
          DebugState.Pause
        }
      }

      object SetDiffExcludes extends DebugCommand {
        val name = "diffExcludes"
        val desc = "set info commands to exclude in the 'info diff' commanad"
        val longDesc = """|Usage: set diffExcludes|de <commands...>
                          |
                          |Set info comamnds to exclude in the 'info diff' command. Multiple arguments
                          |separated by a space excludes multiple commands. Zero arguments excludes no
                          |commands.
                          |
                          |Example: set diffExcludes bitPosition bitLimit""".stripMargin
        override lazy val short = "de"

        override def validate(args: Seq[String]): Unit = {
          val unknown = args.filter { arg =>
            !Info.InfoDiff.infoDiffables.exists(_.matches(arg))
          }
          if (unknown.size > 0) {
            throw new DebugException(
              "unknown or undiffable info commands: " + unknown.mkString(", ")
            )
          }
        }

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.diffExcludes = args
          DebugState.Pause
        }
      }

      object SetInfosetLines extends DebugCommand with DebugCommandValidateInt {
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

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.infosetLines = args.head.toInt
          DebugState.Pause
        }
      }

      object SetInfosetParents extends DebugCommand with DebugCommandValidateInt {
        val name = "infosetParents"
        val desc =
          "set the number of parent elements to show when displaying the infoset (default: -1)"
        val longDesc = """|Usage: set infosetParents|ip <value>
                          |
                          |Set the number of parent elements to show when displaying the infoset.
                          |This only affects the 'info infoset' command. A value of zero will only
                          |show the current infoset element. A value of -1 will show the entire
                          |infoset. Defaults to -1.
                          |
                          |Example: set infosetParents 2""".stripMargin
        override lazy val short = "ip"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.infosetParents = args.head.toInt
          DebugState.Pause
        }
      }

      object SetRemoveHidden extends DebugCommand with DebugCommandValidateBoolean {
        val name = "removeHidden"
        val desc =
          "set whether or not to remove Daffodil internal attributes when displaying the infoset (default: false)"
        val longDesc = """|Usage: set removeHidden|rh <value>
                          |
                          |Set whether or not hidden elements (e.g through the use of
                          |dfdl:hiddenGroupRef) should be displayed. This effects the 'eval' and
                          |'info infoset' commands. <value> must be either true/false or 1/0.
                          |Defaults to false.
                          |
                          |Example: set removeHidden true""".stripMargin
        override lazy val short = "rh"

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          val state = args.head
          DebuggerConfig.removeHidden = if (state == "true" || state == "1") {
            true
          } else {
            false
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

        override def validate(args: Seq[String]): Unit = {
          if (args.size != 1) {
            throw new DebugException("a single argument is required")
          } else {
            args.head.toLowerCase match {
              case "text" =>
              case "binary" =>
              case _ => throw new DebugException("argument must be either 'text' or 'binary'")
            }
          }
        }

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.representation = args.head.toLowerCase match {
            case "text" => Representation.Text
            case "binary" => Representation.Binary
          }
          DebugState.Pause
        }
      }

      object SetWrapLength extends DebugCommand with DebugCommandValidateInt {
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

        def act(
          args: Seq[String],
          state: ParseOrUnparseState,
          processor: Processor
        ): DebugState.Type = {
          DebuggerConfig.wrapLength = args.head.toInt
          DebugState.Pause
        }
      }
    }

    object Step extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "step"
      val desc = "execute a single parser step"
      val longDesc = """|Usage: s[tep]
                        |
                        |Perform a single parse action, pause parsing, and display a debugger
                        |prompt.""".stripMargin
      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebugState.Step
      }
    }

    object Trace extends DebugCommand with DebugCommandValidateZeroArgs {
      val name = "trace"
      val desc = "same as continue, but runs display commands during every step"
      val longDesc = """|Usage: t[race]
                        |
                        |Continue parsing the input data until a breakpoint is encountered,
                        |while running display commands after every parse step. When a
                        |breakpoint is encountered, pause parsing and display a debugger
                        |console to the user.""".stripMargin
      def act(
        args: Seq[String],
        state: ParseOrUnparseState,
        processor: Processor
      ): DebugState.Type = {
        DebugState.Trace
      }
    }
  }
}

/**
 * A stub OOLAGHost is needed to accumulate warnings that may be created
 * during expression compilation in the debugger.
 */
class DebuggerHost(override val tunable: DaffodilTunables)
  extends OOLAGHostImpl(null) // null means this is the root OOLAG Host
  with BasicComponent {

  /**
   * As seen from class DebuggerHost, the missing signatures are as follows.
   *  *  For convenience, these are usable as stub implementations.
   */
  // Members declared in org.apache.daffodil.lib.xml.ResolvesQNames
  def namespaces: scala.xml.NamespaceBinding = ???
  def noPrefixNamespace: NS = ???
  def unqualifiedPathStepPolicy: org.apache.daffodil.lib.iapi.UnqualifiedPathStepPolicy = ???
  // Members declared in org.apache.daffodil.lib.exceptions.ThrowsSDE
  def schemaFileLocation: org.apache.daffodil.lib.exceptions.SchemaFileLocation = ???

  override lazy val localSuppressSchemaDefinitionWarnings: Seq[WarnID] = Seq()
}
