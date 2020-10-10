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
object ContinuationPassing {

  // Example 1 -- Not continuation passing style
  def add( x : Int, y : Int, z : Int ) : Int = {
    x + y + z
  }
  def mult( x : Int, y : Int ) : Int = {
    x * y
  }
  def multAdd( x : Int, y : Int, z : Int ) : Int = {
    add( mult( x, y ), y, z )
  }

  // Example 1 -- Continuation passing style
  def addK( x : Int, y : Int, z : Int, k : Int => Int ) : Int = {
    k( x + y + z ) // (4) Pass to k=x=>x: x=v1=x*y, y=y_multAddK, z=z_multAddK
  }
  def multK( x : Int, y : Int, k : Int => Int ) : Int = {
    k( x * y )  // (2) Multiply x*y, pass the result to k1
  }
  def multAddK( x : Int, y : Int, z : Int, k : Int => Int ) : Int = {
    def k1( v1 : Int ) : Int = {
      addK( v1, y, z, k )  // (3) Call addK, v1=x*y, k=x=>x
    }
    multK( x, y, k1 )  // (1) Call multK, pass continuation k1
  }

}
/* -------------------------------------------------------------------------------------------------------------- */

/**
 *  Continuation Passing Style
 *
 *    (1) Every function has a "continuation" argument
 *
 *    (2) Continuation: a function that specifies what the caller wishes to do with the computed result
 *
 *    (3) The continuation function is passed the result of computation, and can do further computation
 *
 * EX:
 *
 *    def func( x : Int ) : Int = {      --|
 *        ...                              |____ Standard function
 *        return result                    |
 *    }                                  --|
 *
 *    def func( x : Int, k : Int => Int ) : Int = {    --|
 *        ...                                            |____ Continuation passing style function
 *        k( result )                                    |
 *    }                                                --|
 *
 *  (4) Argument ( k : Int => Int ) is a continuation; it takes the result and specifies what to do with it.
 *
 *
 *
 *
 */
object Notes {
  def main( args : Array[ String ] ) : Unit = {

    /**
     * Overall goal: Multiply first two numbers (x * y), then sum all numbers
     *
     * (1) 'multAddK' defines continuation 'k1' then calls 'multK':
     *
     *             multAddK( 1, 2, 3, x => x )  ~~>  multK( 1, 2, k1(v)=addK(v, 2, 3, k1) )

     * (2) 'multK' calls the continuation 'k1':
     *
     *          multK( 1, 2, k1(v)=addK(v, 2, 3, k1) )  ~~>  k1( 1 * 2 ) ~~> addK( 2, 2, 3, k = x => x )
     *
     * (3) addK calls the continuation 'k':
     *
     *            addK( 2, 2, 3, k = x => x )  ~~>  k( (2 + 2 + 3) => ( 2 + 2 + 3 ) )
     *                                    k( 7 => 7 ) ~~> 7
     */
    println( ContinuationPassing.multAddK( 1, 2, 3, x => x ) )  // Out: 7

  }
}