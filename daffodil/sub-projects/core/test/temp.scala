val n = Console.readLine.toInt
for(i <- 0 until n){
  for(j <- 1 until n)
    print((i*n+j)+",")
  println(i*n+n)
}
