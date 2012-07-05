package models

import collection.mutable.Map

case class BKTreeNode[T](v:T,c: Map[Int,BKTreeNode[T]] = Map.empty[Int,BKTreeNode[T]]) {
  def +(cv: T) {
    val d = Distance(v.toString,cv.toString)
    c.get(d) match {
	case Some(cn) => cn + cv
	case _ => c(d) = BKTreeNode(cv)
    }
  }
  def ?(cv:T,n:Int=1) : List[(Int,T)] = {
    val d = Distance(v.toString,cv.toString)
    val min = math.abs(d - n)
    val max = d + n
    println("v=%s,min=%s,max=%s".format(v,min,max))
    val result = (min to max) flatMap { c.get(_) match {
	  case Some(x @ BKTreeNode(_v,_)) => {
	      val d = Distance(cv.toString,_v.toString)
	      if(d <= n) { (d,_v) :: x ? (cv,n) } else { x ? (cv,n) }
	  }
	  case _ => Nil
       }
    }
    result.toList
  }
}

class BKTree[T] {
  var node : Option[BKTreeNode[T]] = None
  def +(v:T) = node match {
    case Some(x @ BKTreeNode(_,_)) => x + v
    case _ => node = Some(BKTreeNode(v))
  }
  def ?(v:T,n:Int=1) = node match {
    case Some(x @ BKTreeNode(_,_)) => x ? (v,n)
    case _ => Nil
  }
}
