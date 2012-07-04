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
}

class BKTree[T] {
  var node : Option[BKTreeNode[T]] = None
  def +(v:T) = node match {
    case Some(x @ BKTreeNode(_,_)) => x + v
    case _ => node = Some(BKTreeNode(v))
  }
}
