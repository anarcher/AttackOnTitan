package models
import collection.mutable.Map
import collection.mutable.ListBuffer

case class Movie(
    Plot:String,
    Rated:String,
    Title:String,
    Poster:String,
    Writer:String,
    imdbRating:String,
    Director:String,
    Released:String,
    Actors:String,
    imdbVotes:String,
    Year:String,
    Genre:String,
    Runtime:String,
    imdbID:String) {
  override def toString = {
    Title
  }
}


class Movies {
  val tree = new BKTree[String]()
  val wordMap = Map.empty[String,ListBuffer[Movie]]
  def add(m:Movie) {
    for(w <- m.Title.split(" ")) {
      wordMap.get(w) match {
	  case Some(x) => x.append(m)
	  case None => {
	    val lb = new ListBuffer[Movie]
	    lb.append(m)
	    wordMap(w) = lb
	  }
      }
    }
  }
  def buildTree() {
    for(w <- wordMap) {  tree + w._1  }
  }
}

object Movies {
  val movies = new Movies()

  def fromFile(fileName: String) {
    import java.io.{BufferedReader,FileReader}
    import com.codahale.jerkson.Json.stream
    val reader = new BufferedReader(new FileReader(fileName))
    for(m <- stream[Movie](reader)) {
       movies.add(m)
    }
    movies.buildTree()
  }
}
