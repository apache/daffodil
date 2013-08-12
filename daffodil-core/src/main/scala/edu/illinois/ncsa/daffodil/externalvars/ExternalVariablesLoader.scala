package edu.illinois.ncsa.daffodil.externalvars

import scala.xml.parsing.ConstructingParser
import java.io.File
import java.net.URI
import scala.xml.Node
import scala.io.Codec.string2codec
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc._
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import org.jdom.Element
import edu.illinois.ncsa.daffodil.xml.XMLUtils

object ExternalVariablesLoader {

  def createBindings(vars: Map[String, String]): Seq[Binding] = {
    val varsKVP = vars.map {
      case (name, value) => {
        Binding(name, value)
      }
    }.toSeq
    varsKVP
  }

  def getVariablesAsBindings(vars: Map[String, String]): Seq[Binding] = {
    val bindings = createBindings(vars)
    bindings
  }

  def getVariablesNodeAsBindings(node: Node): Seq[Binding] = getBindings(node)

  def getVariablesFileAsBindings(varsFile: File): Seq[Binding] = {
    val node = getVariablesByFileAsNode(varsFile)
    val bindings = this.getBindings(node)
    bindings
  }

  def getVariablesSourceAsNode(source: AnyRef): Node = {
    val finalNode = source match {
      case fileName: String => getVariablesByFileNameAsNode(fileName)
      case uri: URI => getVariablesByURIAsNode(uri)
      case file: File => getVariablesByFileAsNode(file)
      case node: Node => node
      case jdomElt: Element => {
        val node = XMLUtils.element2Elem(jdomElt)
        node
      }
      case _ => throw new Exception("Failed to retrieve external variables.  Unrecognized source (%s).  Must be fileName, URI, File, JDOM Element or Node.".format(source))
    }
    finalNode
  }

  def getVariablesByFileAsNode(file: File): Node = {
    Assert.usage(file != null, "loadVariablesByFile expects 'file' to not be null!")
    ExternalVariablesValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file)
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    node
  }

  def getVariablesByFileNameAsNode(fileName: String): Node = {
    Assert.usage(fileName != null, "loadVariablesByFileName expects 'fileName' to not be null!")
    val f = new File(fileName)
    getVariablesByFileAsNode(f)
  }

  def getVariablesByURIAsNode(uri: URI): Node = {
    Assert.usage(uri != null, "loadVariablesByURI expects 'uri' to not be null!")
    val file = new File(uri)
    getVariablesByFileAsNode(file)
  }
  
  def loadVariablesByMap(vars: Map[String,String], referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    val bindings = getVariablesAsBindings(vars)
    val finalVMap = this.loadVariablesByBinding(bindings, referringContext, vmap)
    finalVMap
  }

  def loadVariablesByFileName(fileName: String, referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    Assert.usage(fileName != null, "loadVariablesByFileName expects 'fileName' to not be null!")
    val f = new File(fileName)
    loadVariablesByFile(f, referringContext, vmap)
  }

  def loadVariablesByURI(uri: URI, referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    Assert.usage(uri != null, "loadVariablesByURI expects 'uri' to not be null!")
    val file = new File(uri)
    loadVariablesByFile(file, referringContext, vmap)
  }

  def loadVariablesByFile(file: File, referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    Assert.usage(file != null, "loadVariablesByFile expects 'file' to not be null!")
    ExternalVariablesValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file)
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    loadVariablesByNode(node, referringContext, vmap)
  }

  def loadVariablesByNode(node: Node, referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    Assert.usage(node != null, "loadVariablesByNode expects 'node' to not be null!")
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val bindings = getBindings(node)
    loadVariablesByBinding(bindings, referringContext, vmap)
  }

  def loadVariablesByBinding(bindings: Seq[Binding], referringContext: ThrowsSDE, vmap: VariableMap = EmptyVariableMap): VariableMap = {
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val finalVMap = VariableMap.setExternalVariables(vmap, bindings, referringContext)
    finalVMap
  }

  private def getBindings(extVarBindings: Node) = {
    val bindings = extVarBindings \ "bind"
    bindings.map(b => new Binding(b))
  }

}
