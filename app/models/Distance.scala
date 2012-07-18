package models

import math.min


object Distance {
  def minimum(x:Int*) = {
     x.reduce((l,r) => min(l,r))
  }
  def apply(s:String,t:String) : Int = {
    val m = s.length 
    val n = t.length 
    val d = Array.ofDim[Int](m+1, n+1)
    for (i <- 0 to m) d(i)(0) = i
    for (j <- 0 to n) d(0)(j) = j
    for (j <- 1 to n) {
      for(i <- 1 to m) {
	  d(i)(j) = if(s(i-1) == t(j-1)) { d(i-1)(j-1) } else {
	   minimum(d(i-1)(j)+1,d(i)(j-1)+1,d(i-1)(j-1)+1)
          }
	  //println("i=%s,j=%s,d=%s".format(i,j,d(i)(j)))
      }
    }
    //println("m=%s,n=%s,d=%s".format(m,n,d(m)(n)))
    d(m)(n)
  }

}


// vim: set ts=4 sw=4 et:
