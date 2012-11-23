package daffodil.arguments

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/* 
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

/**
 * Utility class for parsing command line arguments.
 *
 * Usage: add description of the arguments to be parsed to an instance of this class, then invoke 'parse' on a command
 * line and then get the values of the arguments by name.
 *
 * @author Alejandro Rodriguez
 * @version 1
 */
class ArgumentParser {

  private var argumentTypes = List[ArgumentDescription]()

  private var values = Map[String, ArgumentValue]()

  /** Add the descrption of an argument to be accepted */
  def add(argument: ArgumentDescription) = {
    if (argumentTypes exists { x: ArgumentDescription =>
      x.key == argument.key ||
        x.longName == argument.longName ||
        x.shortName == argument.shortName
    })
      throw new IllegalArgumentException("Already added an argument with that name")
    else
      argumentTypes = argument :: argumentTypes
  }

  /** Parses a command line, set the state of this object with the values parsed */
  def parse(commandLine: Array[String]): Unit = {
    processArguments(commandLine)
    argumentTypes.filter {
      _.number match {
        case RequiredSingle | RequiredMultiple => true;
        case _ => false
      }
    }.foreach { x: ArgumentDescription =>
      get(x) match {
        case UnsetValue => throw new IllegalArgumentException("Required argument not set [" + x.longName + "]")
        case _ =>
      }
    }
  }

  /**
   * Returns whether the value has been set by the command line parsed
   * @param key The name of the argument
   */
  def isSet(key: String): Boolean = {
    values get (key) match {
      case Some(s) => true
      case None => false
    }
  }

  /** Returns the value of an argument */
  def get(option: ArgumentDescription): ArgumentValue = {
    values get (option.key) match {
      case Some(x) => x
      case None =>
        if (option.argument) {
          option.default match {
            case Some(s) => option.number match {
              case RequiredSingle | OptionalSingle => new SingleValue(s)
              case RequiredMultiple | OptionalMultiple => new MultipleValue(List(s))
            }
            case None => UnsetValue
          }
        } else
          UnsetValue
    }
  }

  /**
   * Returns the value of an argument
   * @param key the name of the argument
   */
  def get(key: String): ArgumentValue = {

    val option = argumentTypes.find { _.key == key }

    option match {
      case Some(o) => get(o)
      case None => throw new IllegalArgumentException("Option not set [" + key + "]")
    }

  }

  /**
   * Returns the value of a single-valued argument as a string
   * @param key the name of the argument
   */
  def getSingle(key: String): String = {
    get(key) match {
      case SingleValue(v) => v
      case _ => null
    }
  }

  /**
   * Returns the values of a multi-valued argument as a list of strings
   * @param key the name of the argument
   */
  def getMultiple(key: String): List[String] = {
    get(key) match {
      case MultipleValue(v) => v
      case _ => null
    }
  }

  private def processArguments(toSet: Array[String]): Unit =
    if (toSet.size > 0) {
      val fullHeader = toSet(0)
      var shortVersion = false

      val optionName: Option[ArgumentDescription] =
        if (fullHeader startsWith ("--")) {
          val header = fullHeader.substring(2)
          argumentTypes.find { _.longName == header }
        } else if (fullHeader startsWith ("-")) {
          val header = fullHeader.substring(1, 2)
          shortVersion = true
          argumentTypes.find { _.shortName == header }
        } else
          throw new IllegalArgumentException("Invalid option [" + fullHeader + "]")

      optionName match {
        case Some(option) => processArguments(processSingleArgument(option, toSet, shortVersion))
        case None => throw new IllegalArgumentException("Argument not valid [" + fullHeader + "]")
      }
    }

  private def processSingleArgument(option: ArgumentDescription, toSet: Array[String],
                                    shortVersion: Boolean): Array[String] = {
    if (option.argument) {
      if (shortVersion) {
        if (toSet(0).length > 2) {
          addValue(option, toSet(0).substring(2))
          toSet drop (1)
        } else if (toSet.length > 1) {
          addValue(option, toSet(1))
          toSet drop (2)
        } else
          throw new IllegalArgumentException("Missing argument for option [" + toSet(0) + "]")
      } else {
        if (toSet.length > 1) {
          addValue(option, toSet(1))
          toSet drop (2)
        } else
          throw new IllegalArgumentException("Missing argument for option [" + toSet(0) + "]")
      }
    } else {
      addValue(option)
      toSet drop (1)
    }
  }

  private def addValue(option: ArgumentDescription): Unit = {
    option.number match {
      case RequiredSingle | OptionalSingle => values.get(option key) match {
        case None => values = values ++ Map(option.key -> NoValue)
        case _ => throw new IllegalArgumentException("Multiple values set for option [" + option.longName + "]")
      }
      case RequiredMultiple | OptionalMultiple => values.get(option key) match {
        case None | Some(NoValue) => values ++ Map(option.key -> NoValue)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  private def addValue(option: ArgumentDescription, value: String): Unit = {
    option.number match {
      case RequiredSingle | OptionalSingle => values.get(option key) match {
        case None => values = values ++ Map(option.key -> new SingleValue(value))
        case _ => throw new IllegalArgumentException("Multiple values set for option [" + option.longName + "]")
      }
      case RequiredMultiple | OptionalMultiple => values.get(option key) match {
        case Some(MultipleValue(list)) => values = values ++ Map(option.key -> new MultipleValue(list ::: List(value)))
        case None => values = values ++ Map(option.key -> new MultipleValue(List(value)))
        case _ => throw new IllegalArgumentException
      }
    }
  }

}