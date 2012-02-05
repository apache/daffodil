val n = Console.readLine.toInt
println("<matrix xmlns=\"http://www.example.org/example1/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\">")
for(i <- 0 until n){
  println("  <row>")
    for(j <- 1 to n){
      print("    <cell type=\"xsd:int\">")
      print(i*n+j)
      println("</cell>")
    }
  println("  </row>")
}
println("</matrix>")
