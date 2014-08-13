package edu.illinois.ncsa.daffodil.saxon

//import edu.illinois.ncsa.daffodil.processors.xpath.NodeResult
//import edu.illinois.ncsa.daffodil.processors.xpath.StringResult
//import javax.xml.xpath.{ XPathConstants, XPathException }
//import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
//import edu.illinois.ncsa.daffodil.processors.xpath.NotANumberResult
//import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
//import edu.illinois.ncsa.daffodil.processors.VariableMap
//import edu.illinois.ncsa.daffodil.processors.xpath.NodeSetResult
//import edu.illinois.ncsa.daffodil.processors.PState
//import edu.illinois.ncsa.daffodil.processors.xpath.BooleanResult
//import edu.illinois.ncsa.daffodil.processors.Infoset
//import edu.illinois.ncsa.daffodil.dsom.R
//import edu.illinois.ncsa.daffodil.processors.RuntimeData
//import edu.illinois.ncsa.daffodil.processors.xpath.NumberResult
//import edu.illinois.ncsa.daffodil.processors.InfosetElement
//import edu.illinois.ncsa.daffodil.dsom.TypeConversions
//import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
//import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
//import edu.illinois.ncsa.daffodil.exceptions.Assert
//import scala.collection.immutable.Queue
//import edu.illinois.ncsa.daffodil.processors.xpath.SaxonFunctionState

//class RuntimeExpressionSaxon[T <: AnyRef](convertTo: ConvertToType.Type,
//  xpathText: String,
//  xpathExprFactory: CompiledExpressionFactory,
//  rd: RuntimeData)
//  extends CompiledExpression(xpathText)
//  with WithParseErrorThrowing
//  with TypeConversions {
//
//  override val context = rd
//  def isConstant = false
//  def isKnownNonEmpty = true // expressions are not allowed to return empty string
//  def constant: T = Assert.usageError("Boolean isConstant is false. Cannot request a constant value.")
//
//  def toXPathType(convertTo: ConvertToType.Type) =
//    convertTo match {
//      case ConvertToType.Long => XPathConstants.NUMBER
//      case ConvertToType.Double => XPathConstants.NUMBER
//      case ConvertToType.Int | ConvertToType.UInt | ConvertToType.Integer |
//        ConvertToType.UInteger | ConvertToType.Short | ConvertToType.UShort => XPathConstants.NUMBER
//      case ConvertToType.Decimal | ConvertToType.Byte | ConvertToType.UByte | ConvertToType.ULong => XPathConstants.NUMBER
//      case ConvertToType.String => XPathConstants.STRING
//      case ConvertToType.Element => XPathConstants.NODE
//      case ConvertToType.Boolean => XPathConstants.BOOLEAN
//      case _ => Assert.invariantFailed("convertTo not valid value: " + convertTo)
//    }
//
//  val cePathRegex = """\{(.*)\}""".r

/* TODO: Reimplement this code using the new DPath compiler. DFDL-1010
 * 
 * This code can't work right. These checks need to be done looking at the 
 * static context, i.e., the schema components, to determine if a path makes sense.
 * 
  def checkForUnorderedSeqAndChoiceBranchViolations(
    expr: String, pre: InfosetElement, vmap: VariableMap, pstate: PState): VariableMap = {

    // We want to be able to determine if we're in a choice or unordered sequence
    var variables = vmap



    // The element on which the expression is situated.
    val preElementBase = pre.schemaComponent(pstate)
    val shouldCheckBranch: Boolean =
      if (preElementBase.inUnorderedSequence || preElementBase.inChoiceBeforeNearestEnclosingSequence) true
      else false

    if (shouldCheckBranch) {
      val dfdlExprCompiler = new DFDLPathExpressionCompiler(preElementBase)

      val result =
        try {
          variables.currentPState = Some(pstate) // Provide pstate here to issue runtime errors
          dfdlExprCompiler.getPathsFromExpressionAsCompiledExpressions(expr, variables)
        } finally { variables.currentPState = None }

      result match {
        case Right((paths, newVMap)) => {
          paths.foreach(p => {

            val path = p.prettyExpr match {
              // Attempt to extract just the expression without the
              // curly braces
              case cePathRegex(pathText) => pathText.trim
              case _ => p.prettyExpr
            }
            val preParent = pre.jdomElt.get.getParentElement()
            val (optInfosetElems, newerVMap) = p.evaluatesToNodes(pre, variables, pstate)

            val isReferringToSelf: Boolean = path == "."

            if (!isReferringToSelf && optInfosetElems.isDefined) {
              val elems = optInfosetElems.get
              elems.foreach(e => {
                val evalElementBase = e.schemaComponent(pstate)
                checkElementForBranchViolations(preElementBase, evalElementBase, expr, path, pstate)
              })
            }
            variables = newerVMap
          })
          variables = newVMap
        }
        case Left((msg, newVMap)) => {
          // Failed, do we care? Won't the rest of "evaluate" then fail properly?
          variables = newVMap
        }
      }
    }
    variables
  }

  def checkElementForBranchViolations(preElementBase: ElementBase, evalElementBase: ElementBase,
    expr: String, path: String, context: ThrowsSDE) = {

    val preNearestEncUOSeq = preElementBase.nearestEnclosingUnorderedSequence
    val preNearestEncChoice = preElementBase.nearestEnclosingChoiceBeforeSequence
    val evalNearestEncChoice = evalElementBase.nearestEnclosingChoiceBeforeSequence
    val evalNearestEncUOSeq = evalElementBase.nearestEnclosingUnorderedSequence

    if (preElementBase.inChoiceBeforeNearestEnclosingSequence) {
      // rootedElem aka preElementBase was on a choice branch, is the node returned
      // via evaluation on a different branch?

      if (evalNearestEncChoice.isDefined) {
        val evalChoice = evalNearestEncChoice.get
        val preChoice = preNearestEncChoice.get

        // Is this a different choice?
        if (evalChoice.mgID != preChoice.mgID) {
          context.SDE("Expression %s contains a path (%s) which navigates to a branch of another choice (%s)",
            expr, path, evalChoice.path)
        }

        // Same Choice, but is it the same branch?
        if (evalElementBase.tID != preElementBase.tID) {
          context.SDE("Expression %s contains a path (%s) which navigates to another branch (%s) of the same choice.",
            expr, path, evalElementBase)
        }
      }
      if (evalNearestEncUOSeq.isDefined) {
        context.SDE("Expression %s contains a path (%s) which navigates from a choice (%s) to member (%s) an enclosing unordered group (%s)",
          expr, path, preElementBase, evalElementBase, evalNearestEncUOSeq.get)
      }
    } else {
      // rootedElem aka preElementBase is in an UnorderedSeq

      // evalElem is in a Choice before an UnorderedSeq, but preElem is in an UnorderedSeq.
      if (evalNearestEncChoice.isDefined) {
        context.SDE("Expression %s contains a path (%s) which navigates from a member (%s) of an unordered group to a choice branch (%s)",
          expr, path, preElementBase, evalElementBase)
      }
      // evalElem is in UnorderedSeq before a Choice and so is preElem
      if (evalNearestEncUOSeq.isDefined) {
        // Unordered Seq defined for both eval and pre
        val evalUOSeq = evalNearestEncUOSeq.get
        val preUOSeq = preNearestEncUOSeq.get

        // Are they the same UnorderedSeq?
        if (evalUOSeq.mgID != preUOSeq.mgID) {
          context.SDE("Expression %s contains a path (%s) which navigates to a branch of another unordered group (%s)",
            expr, path, evalElementBase)
        }
      }

    }
  }
*/

/**
 * For use in cases where we expect the expression to evaluate
 * to a Node or NodeList.
 */
//  def evaluatesToNodes(pre: InfosetElement, vmap: VariableMap, pstate: PState): (Option[List[InfosetElement]], VariableMap) = {
//
//    val xpathResultType = toXPathType(ConvertToType.Element) // We want to always evaluate to a Node here.
//
//    var variables = vmap
//
//    val xpathRes = try {
//      SaxonFunctionState.currentPState.set(Some(pstate))
//      variables.currentPState = Some(pstate)
//      pre.evalExpression(xpathText, xpathExprFactory, variables, xpathResultType)
//    } catch {
//      case e: XPathException => {
//        // runtime processing error in expression evaluation
//        val ex = if (e.getMessage() == null) e.getCause() else e
//        PE("Expression evaluation failed. Details: %s", ex)
//      }
//    } finally {
//      SaxonFunctionState.currentPState.set(None) // put it back off
//      variables.currentPState = None
//    }
//
//    val newVariableMap = xpathExprFactory.getVariables() // after evaluation, variables might have updated states.
//    val result: Option[List[InfosetElement]] = xpathRes match {
//      case NodeResult(n) => {
//        val elem = Infoset.newElement(n)
//        Some(List(elem))
//      }
//      case NodeSetResult(ns) => {
//        val numNodes = ns.getLength()
//        val q: Queue[InfosetElement] = Queue.empty
//        for (i <- 0 to numNodes) {
//          val item = ns.item(i).asInstanceOf[org.jdom2.Element]
//
//          val elem = Infoset.newElement(item)
//          q.enqueue(elem)
//        }
//        Some(q.toList)
//      }
//      case _ => None
//    }
//    (result, newVariableMap)
//  }
//
//  def evaluate(pre: InfosetElement, vmap: VariableMap, pstate: PState): R = {
//    val xpathResultType = toXPathType(convertTo)
//
//    var variables = vmap // checkForUnorderedSeqAndChoiceBranchViolations(xpathText, pre, vmap, pstate) DFDL-1010
//
//    val xpathRes = try {
//      SaxonFunctionState.currentPState.set(Some(pstate))
//      variables.currentPState = Some(pstate)
//      pre.evalExpression(xpathText, xpathExprFactory, variables, xpathResultType)
//    } catch {
//      case e: XPathException => {
//        // runtime processing error in expression evaluation
//        val ex = if (e.getMessage() == null) e.getCause() else e
//        PE("Expression evaluation failed. Details: %s", ex)
//      }
//    } finally {
//      SaxonFunctionState.currentPState.set(None) // put it back off
//      variables.currentPState = None
//    }
//
//    val newVariableMap = xpathExprFactory.getVariables() // after evaluation, variables might have updated states.
//    val converted: T = xpathRes match {
//      case NotANumberResult(v) => {
//        PE("Expression %s evaluated to something that is not a number (NaN): %s.", xpathText, v)
//      }
//      case NumberResult(n) => {
//        val convertedResult = try {
//          convertTo match {
//            case ConvertToType.Long => convertToLong(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Double => convertToDouble(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Int => convertToInt(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Byte => convertToByte(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.UByte => convertToUByte(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Short => convertToShort(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.UShort => convertToUShort(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.UInt => convertToUInt(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Boolean => convertToBoolean(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.ULong => convertToULong(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Integer => convertToInteger(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.Decimal => convertToDecimal(n.toString, pstate).asInstanceOf[T]
//            case ConvertToType.UInteger => convertToNonNegativeInteger(n.toString, pstate).asInstanceOf[T]
//            case _ => n.asInstanceOf[T]
//          }
//        } catch {
//          // Note that the converToXXX functions all SDE themselves on conversion errors.
//          // Here we just want to catch the exceptions that are the result of asInstanceOf[T]
//          // or other unforeseen exceptions.
//          case u: UnsuppressableException => throw u
//          case cex: ClassCastException => pstate.SDE("Cannot convert %s to %s. Error %s", n, convertTo, cex)
//        }
//        convertedResult
//      }
//      case StringResult(s) => {
//        Assert.invariant(convertTo == ConvertToType.String)
//        s.asInstanceOf[T]
//      }
//      case BooleanResult(v) => {
//        Assert.invariant(convertTo == ConvertToType.Boolean)
//        v.asInstanceOf[T]
//      }
//      case NodeResult(n) => {
//        n.asInstanceOf[T]
//      }
//      case NodeSetResult(ns) => {
//        throw new Exception("NodeSet should not be returned except when used inside an xpath function.")
//      }
//    }
//    (converted, newVariableMap)
//  }
//}
