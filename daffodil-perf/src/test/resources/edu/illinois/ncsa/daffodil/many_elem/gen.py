#!/usr/bin/python

for i in range(1001,9999):
  print "<xs:element name='e%04d' type='xs:string' dfdl:lengthKind='delimited'/>" % i

exit(0)
