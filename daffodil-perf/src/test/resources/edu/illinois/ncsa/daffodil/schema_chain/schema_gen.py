#!/usr/bin/python

num = 100

for i in range(1,num+1):
  buff=""
  f = open('chain_%03d.dfdl.xsd' % i, 'w')
  buff += '<?xml version="1.0" encoding="UTF-8"?>\n'
  buff += '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">\n\n'
#  buff += ' targetNamespace="http://s%03d.com" xmlns:s%03d="http://s%03d.com" xmlns:s%03d="http://s%03d.com">\n' % (i, i, i, i+1, i+1)
  buff += ' <xs:annotation>\n    <xs:appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">\n     <dfdl:format separator="" initiator="" terminator="" leadingSkip="0" textTrimKind="none" initiatedContent="no"\n        ignoreCase="no" representation="text" textNumberRep="standard" lengthKind="delimited" encoding="ASCII"/>\n    </xs:appinfo>\n </xs:annotation>\n\n'

  if i < num: 
    buff += ' <xs:include schemaLocation="edu/illinois/ncsa/daffodil/schema_chain/chain_%03d.dfdl.xsd"/>\n\n' % (i+1)

  buff += ' <xs:element name="e%03d" type="xs:string" dfdl:lengthKind="delimited"/>\n\n' % i
  buff += '</xs:schema>\n'
  f.write(buff)
  f.close()

exit(0)
