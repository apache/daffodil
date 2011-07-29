package daffodil.processors.input

import org.jdom.Element
import org.jdom.Parent

import daffodil.debugger.DebuggingListener
import daffodil.schema.annotation.Annotation
import daffodil.xml.Namespaces
import daffodil.parser.RollbackStream
import daffodil.parser.regex.Regex
import daffodil.processors.{ScanResult, VariableMap}

class DebuggingBasicProcessor(annotation:Annotation,debugger:DebuggingListener,
                         processor:BasicProcessor) extends BasicProcessor {
  
  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,namespaces:Namespaces,
  					 terminators:List[Regex]) = {
    debugger step(annotation element,annotation,element,variables,namespaces,input)
    processor(input,element,variables,namespaces,terminators)
  }
  
  override def init(input:RollbackStream,element:Element,variables:VariableMap,
                    namespaces:Namespaces) = {
    debugger step(annotation element,annotation,element,variables,namespaces,input)
    processor init(input,element,variables,namespaces)
  }
  
  override def terminate(input:RollbackStream,element:Element,variables:VariableMap,
                         namespaces:Namespaces,terminators:List[Regex]) = {
    debugger step(annotation element,annotation,element,variables,namespaces,input)
    processor terminate(input,element,variables,namespaces,terminators)
  }
  
   def findPrefixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = {
     debugger step(annotation element,annotation,parent,variables,namespaces,input)
     processor findPrefixSeparator(input,parent,variables,namespaces,parentTerminators)
                    	  
   }
  
   def findPostfixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	    namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = {
      debugger step(annotation element,annotation,parent,variables,namespaces,input)
      processor findPostfixSeparator(input,parent,variables,namespaces,parentTerminators)
   }
                    	
}
