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
    for(w <- m.Title.split(" "); _w = w.toLowerCase ) {
      wordMap.get(_w) match {
	  case Some(x) => x.append(m)
	  case None => {
	    val lb = new ListBuffer[Movie]
	    lb.append(m)
	    wordMap(_w) = lb
	  }
      }
    }
  }
  def buildTree() {
    for(w <- wordMap) {  tree + w._1  }
  }
  def searchTree(w:String,n:Int=1) = {
    tree ? (w,n)
  }
  def search(query:String) = {
    def n(word:String,percent:Int=25) = { (word.size.toFloat / 100 * 25).toInt }
    val qw = query.toLowerCase.split(" ")
    val words = qw.map(x => searchTree(x,n(x)))
//    val movies = words.map( xs => xs.flatMap { x => wordMap.get(x._2).get.map((x._1,_))}.distinct)
    val movies = words.map( xs => xs.flatMap { x => wordMap.get(x._2).get }.distinct )
//    println(movies(0).filter( _.Title == "The Dark Knight"))
//    println(movies(1).filter( _.Title == "The Dark Knight"))
    val _movies = movies.tail.foldLeft(movies.head) { (xs,x) => xs.intersect(x) }
    _movies
  }
}

object Movies {
  val movies = new Movies()

  def fromFile(fileName: String) = {
    import java.io.{BufferedReader,FileReader}
    import com.codahale.jerkson.Json.stream
    val reader = new BufferedReader(new FileReader(fileName))
    for(m <- stream[Movie](reader))   movies.add(m)
    movies.buildTree()
    movies
  }

  def search(query:String) = movies.search(query)
}
