package daffodil.parser.xml
import org.jdom.output.Format
import org.jdom.Element

class XMLOutputter(format : Format) extends org.jdom.output.XMLOutputter(format) {

	protected def printElement(
			out: daffodil.parser.xml.Writer,
			element : Element,
			level : Int,
			namespaces : NamespaceStack)  {
		out.setElement(element,level)
		super.printElement(out,element,level,namespaces)
	}
}