package daffodil

import java.io.FileOutputStream
import java.io.FileInputStream
import daffodil.xml.TransformUtil
import scala.io.Source

object TestExamples {
	// must be run in the "test" directory
  def main(args : Array[String]) : Unit = {
	  for (line <- Source.fromFile("testSuite.txt").getLines.toList.filter{!_.startsWith("#")}) {
	 	  val p = line.split(",")
	 	  val (schema, root, input, output) = (p(0),p(1),p(2),p(3))
 		  print("Canonicalizing "+schema+"...")
 		  TransformUtil.transform(new FileInputStream(schema), new FileInputStream("../canonicalize.xsl"), new FileOutputStream("canonical/"+schema))
 		  print("Done. Processing input using canonicalized schema...\n")
 		  Main.main(Array("-s","canonical/"+schema,"-r",root,"-i",input))
 		  print("Done. Processing input using pre-canonicalized schema...\n")
 		  Main.main(Array("-s",schema,"-r",root,"-i",input))
	  }
	  print("All tests complete\n")
  }
}

