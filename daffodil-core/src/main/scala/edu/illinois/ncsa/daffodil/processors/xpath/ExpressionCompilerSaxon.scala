package edu.illinois.ncsa.daffodil.processors.xpath
//
//import edu.illinois.ncsa.daffodil.exceptions._
//import edu.illinois.ncsa.daffodil.processors.xpath._
//import javax.xml.xpath._
//import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
//import edu.illinois.ncsa.daffodil.util._
//import edu.illinois.ncsa.daffodil.util.LogLevel
//import edu.illinois.ncsa.daffodil.xml.XMLUtils
//import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
//import scala.xml.Node
//import scala.collection.JavaConversions._
//import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
//import edu.illinois.ncsa.daffodil.dsom.ConstantExpression
//import edu.illinois.ncsa.daffodil.dsom.ExpressionCompilerBase
//import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
//import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
//import edu.illinois.ncsa.daffodil.dpath._
//import edu.illinois.ncsa.daffodil.processors.RuntimeData
//import scala.xml.NamespaceBinding

//class ExpressionCompilerSaxon(rd: RuntimeData) extends ExpressionCompilerBase(rd) {
//
//  /**
//   * The only way I know to check if the compiled expression was just a constant
//   * is to evaluate it in an environment where if it touches anything (variables, jdom tree, etc.)
//   * it will throw. No throw means a value came back and it must be a constant.
//   *
//   * This handles simple folding like converting { 5 + fn:string-length("foobar") } into 11.
//   */
//  def constantValue(xpathExprFactory: CompiledExpressionFactory): Option[Any] =
//    // withLoggingLevel(LogLevel.Info) 
//    {
//      val dummyVars = EmptyVariableMap
//      val result = try {
//        SaxonFunctionState.currentPState.set(None) // no state if we're trying for a constant value.
//        val res = XPathUtil.evalExpression(
//          xpathExprFactory.expression + " (to see if constant)",
//          xpathExprFactory,
//          dummyVars,
//          null, // context node is not needed to see if an expression is a constant.
//          XPathConstants.STRING) // <-- This looks like it always evaluates to StringResult
//        res match {
//          case StringResult(s) => {
//            log(LogLevel.Debug, "%s is constant", xpathExprFactory.expression)
//            Some(s)
//          }
//          case BooleanResult(s) => {
//            log(LogLevel.Debug, "%s is constant", xpathExprFactory.expression)
//            Some(s)
//          }
//          case _ => Assert.invariantFailed("Can't evaluate to " + res + " when testing for isConstant")
//        }
//      } catch {
//        case u: UnsuppressableException => throw u
//        case e: XPathExpressionException => {
//          log(LogLevel.Debug, "%s is NOT constant (due to %s)", xpathExprFactory.expression, e.toString)
//          None
//        }
//        case e: SchemaDefinitionError => {
//          // TODO differentiate between the xpath being syntax-invalid (hence, an SDE, not a constant/runtime distinction
//          // and other SDEs like variable not defined, which just indicates (for here), that the expression 
//          // is non-constant.
//          log(LogLevel.Debug, "%s is NOT constant (due to %s)", xpathExprFactory.expression, e.toString)
//          None
//        }
//        //          case e : Exception => {
//        //            Assert.invariantFailed("Didn't get an XPathExpressionException. Got: " + e)
//        //          }
//      } finally {
//        SaxonFunctionState.currentPState.set(None)
//      }
//
//      result
//    }
//
//  def compileExpression[T](
//    convertTo: ConvertToType.Type,
//    expr: String,
//    namespaces: NamespaceBinding,
//    scWherePropertyWasLocated: RuntimeData): CompiledExpression = {
//
//    // This is important. The namespace bindings we use must be
//    // those from the object where the property carrying the expression 
//    // was written, not those of the edecl object where the property 
//    // value is being used/compiled. JIRA DFDL-407
//    val xpath = DPathUtil.getExpression(expr)
//    val compiledXPath =
//      try {
//        //
//        // We also want SDEs from expression compilation issued with the 
//        // schema component where the property was found as the file/line information.
//        // (So the user can go there and see the expression.)
//        //
//        // This can happen. The length and occursCount properties CAN be scoped,
//        // or placed on simpleType definitions so their expressions are shared, and
//        // are not on the same lexical object that has that length or occurrances.
//        // 
//        XPathUtil.compileExpression(xpath, namespaces, scWherePropertyWasLocated)
//      } catch {
//        case e: XPathExpressionException => {
//          val exc = e // debugger never seems to show the case variable itself.
//          val realExc = if (e.getCause() != null) e.getCause() else exc
//          // Assert.invariant(realExc != null) // it's always an encapsulation of an underlying error.
//          scWherePropertyWasLocated.SDE("XPath Compilation Error: %s", realExc)
//        }
//      }
//    val cv = constantValue(compiledXPath)
//    val compiledExpression = cv match {
//      case Some(s) => {
//        convertTo match {
//          case ConvertToType.String => new ConstantExpression(s)
//          // Evaluating to an Element when we're a constant makes no sense.
//          // case 'Element => new ConstantExpression(s.asInstanceOf[org.jdom2.Element])
//          case ConvertToType.Element => Assert.usageError("Evaluating to an Element when we're a constant makes no sense.")
//          case ConvertToType.Byte => new ConstantExpression(compileTimeConvertToByte(s))
//          case ConvertToType.UByte => new ConstantExpression(compileTimeConvertToUByte(s))
//          case ConvertToType.Short => new ConstantExpression(compileTimeConvertToShort(s))
//          case ConvertToType.UShort => new ConstantExpression(compileTimeConvertToUShort(s))
//          case ConvertToType.Int => new ConstantExpression(compileTimeConvertToInt(s))
//          case ConvertToType.UInt => new ConstantExpression(compileTimeConvertToUInt(s))
//          case ConvertToType.Long => new ConstantExpression(compileTimeConvertToLong(s))
//          case ConvertToType.ULong => new ConstantExpression(compileTimeConvertToULong(s))
//          case ConvertToType.Double => new ConstantExpression(compileTimeConvertToDouble(s))
//          case ConvertToType.Integer => new ConstantExpression(compileTimeConvertToInteger(s))
//          case ConvertToType.UInteger => new ConstantExpression(compileTimeConvertToNonNegativeInteger(s))
//          case ConvertToType.Decimal => new ConstantExpression(compileTimeConvertToDecimal(s))
//          case ConvertToType.Boolean => new ConstantExpression(compileTimeConvertToBoolean(s))
//        }
//      }
//      case None => new RuntimeExpressionSaxon(convertTo, expr, compiledXPath, scWherePropertyWasLocated)
//    }
//    compiledExpression
//  }
//}