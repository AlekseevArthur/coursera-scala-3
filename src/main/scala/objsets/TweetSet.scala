package objsets

import TweetReader.*

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

abstract class TweetSet extends TweetSetInterface:

  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, Empty())

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:
  def mostRetweeted: Tweet =
    throw java.util.NoSuchElementException("No retweets in the empty list")

  def descendingByRetweet: TweetList = Nil

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:
  def mostRetweeted: Tweet =
    if (right.isInstanceOf[Empty] && left.isInstanceOf[Empty])
      elem
    else if (right.isInstanceOf[Empty])
      compare(left.mostRetweeted, elem)
    else if (left.isInstanceOf[Empty])
      compare(right.mostRetweeted, elem)
    else
      compare(left.mostRetweeted, compare(right.mostRetweeted, elem))

  def descendingByRetweet: TweetList =
    if (left.isInstanceOf[Empty] && right.isInstanceOf[Empty])
      Cons(elem, Nil)
    else
      val mr = mostRetweeted
      Cons(mr, remove(mr).descendingByRetweet)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    if (p(elem))
      right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    else
      right.filterAcc(p, left.filterAcc(p, acc))

  def union(that: TweetSet): TweetSet =
    left.union(right.union(that.incl(elem)))

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

  private def compare(x: Tweet, y: Tweet) =
    if (x.retweets > y.retweets) 
      then x 
      else y


trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => google.exists(w => t.text.contains(w)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(w => t.text.contains(w)))

  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  GoogleVsApple.trending foreach println
