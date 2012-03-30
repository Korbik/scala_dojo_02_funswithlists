package dojo

import collection.immutable.List
import scala.annotation.tailrec

object FunsWithLists {

  def labels(ls: List[Game]) = ls.map(_.label)

  def averageRatingsOf(l: String, ls: List[Game]): Int = {
    val filteredList: List[Game] = ls.filter(_.label == l)
    val length = filteredList.length
    if (length == 0) {
      0
    } else if (length == 1) {
      filteredList.head.rating
    } else {
      val sum = filteredList.foldLeft[Int](0)((acc: Int, g: Game) => acc + g.rating)
      sum / length
    }
  }

  def totalRatingsOf(ls: List[Game]): Int = {
    @tailrec
    def sum(acc: Int, list: List[Game]): Int = list match {
      case Nil => acc
      case x :: xs => sum(acc + x.rating, xs)
    }
    sum(0, ls)
  }

  def totalRatingsOfLabel(label: String, list: List[Game]): Int = {
    var total = 0
    for (g <- list) {
      if (label.equals(g.label)) {
        total += g.rating
      }
    }
    total
  }

  def increaseRatingBy(inc: Int, ls: List[Game]) = ls.map(g => Game(g.label, g.rating + inc))

  def decreaseRatingBy(i: Int, s: String, list: List[Game]) = {
    for (g <- list) yield if (g.label.equals(s)) {
      Game(g.label, g.rating - i)
    } else {
      g
    }
  }

  def createFunctionToFindGamesByLabel(label: String): (List[Game]) => List[Game] = _.filter(g => g.label.equals(label))

  def zipWithKey = (f: (Game) => String, ls: List[Game]) => for (g <- ls) yield (f(g),g)

  def firstGameWithRating (rating:Int) = (list :List[Game]) => list.find(g => g.rating == rating).get

}
