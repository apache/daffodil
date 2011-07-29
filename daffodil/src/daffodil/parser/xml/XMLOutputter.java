package daffodil.parser.xml;

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
import org.jdom.Element;
import org.jdom.output.Format;

import java.io.IOException;


/**
 * A XMLOutputter that when used in conjunction with daffodil.parser.xml.Writer keeps track of where in the output stream
 * each XML element is.
 *
 * @see daffodil.parser.xml.Writer
 * @author Alejandro Rodriguez
 * @version 1
 */
public class XMLOutputter extends org.jdom.output.XMLOutputter {

    //This is written in Java due to scala 2.7 compiler getting confused over
    // the overloading of the printElement method

    public XMLOutputter(Format format) {
        super(format);
    }

    @Override
    protected void printElement(java.io.Writer out,Element element,
                                int level,
                                XMLOutputter.NamespaceStack namespaces) throws IOException {
        if (out instanceof daffodil.parser.xml.Writer)
            printElement((Writer)out,element,level,namespaces);
        else
            super.printElement(out,element,level,namespaces);
    }

    protected void printElement(daffodil.parser.xml.Writer out,
                                Element element,
                                int level,XMLOutputter.NamespaceStack namespaces) throws IOException {
        out.setElement(element,level);
        super.printElement(out,element,level,namespaces);
    }
  
}
