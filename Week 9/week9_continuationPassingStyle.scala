/**
 *  week9_continuationPassingStyle.scala
 */
import scala.annotation.tailrec


object TailRecReview {
  def foo( x : Int ) : Int = {
    x - 15
  }

  @tailrec
  def rec_1( x : Int ) : Int = {
    if ( x <= 0 )
      x
    else
      rec_1( x - 10 )
  }

  @tailrec
  def rec_2( y : Int = 0, x : Int ) : Int = {
    if ( x <= 10 )
      y
    else
      rec_2( y + 1, x - 10 )
  }

  // Not tail recursive
  def rec_3( x : Int ) : Int = {
    if ( x <= 10 )
      x - 10
    else
      1 + rec_3( x - 10 )
  }

  // Not tail recursive
  def rec_4( x : Int ) : Int = {
    if ( x <= 10 )
      x - 5
    else
      rec_4( rec_4( x - 10 ) )
  }

  @tailrec
  def rec_5( x : Int ) : Int = {
    if ( x <= 0 )
      foo( x )
    else
      rec_5( foo( x ) )
  }

  // Not tail recursive
  def rec_6( x : Int ) : Int = {
    if ( x <= 0 )
      foo( x )
    else
      foo( rec_6( x - 5 ) )
  }

  // Not tail recursive
  def factorial( n : Int ) : Int = {
    if ( n <= 0 )
      1
    else
      n * factorial( n - 1 )
  }

  @tailrec
  def factorialAcc( acc : Int = 1, n : Int ) : Int = {
    if ( n <= 0 )
      acc
    else
      factorialAcc( acc * n, n - 1 )
  }

  // Not tail recurisve
  def fibonacci( n : Int ) : Int = {
    if ( n < 2 )
      1
    else
      fibonacci( n - 1 ) + fibonacci( n - 2 )
  }

  @tailrec
  def fibonacciAcc( n : Int, acc1 : Int = 1, acc2 : Int = 1 ) : Int = {
    if (n <= 0)
      acc1
    else if (n == 1)
      acc2
    else
      fibonacciAcc( n - 1, acc2, acc1 + acc2 )
  }
}
/* -------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------------------------------------------- */

object Notes {
  def main( args : Array[ String ] ) : Unit = {
    println( "TODO" )
  }
}