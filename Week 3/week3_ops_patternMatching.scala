/** 
 * File: week3_ops_patternMatching.scala 
 * 
 * Working through the material on operations on inductively defined structures.
 */

 /* Pattern Matching ---------------------------------------------- */


/**
  * Grammar for natural numbers:
  *  
  * Number -> Z | Succ( Number )
  * 
  * { ∅ / 0 } U { { n }/n+1 | n ∈ ℕ } 
  */
sealed trait NatNum;

/* Inductive definition for the set of natural numbers */
case class Z() extends NatNum;                  // Number -> Z i.e. 0
case class Succ( n : NatNum ) extends NatNum;   // Number -> Succ( Number ) i.e. n + 1

/**
 * Operations on inductive definitions using pattern matching
 */
object patternMatching {

  /** 
   * Inductive definition: Set of natural numbers
   * 
   * Match the type of 'NatNum' passed ( Z() or Succ() ) with an operation
   */
  def minusOne( num : NatNum ) : NatNum = {
    
    ( num ) match {

      /* n = 0 */
      case Z() => throw new IllegalArgumentException( "minuxOnePM cannot be called on Zero" );

      /* n = n + 1 */
      case Succ( num ) => return num;

    }
  }
  /* --------------------------------------------------------------- */

  /* Add natural numbers */
  def addNatNums( n1 : NatNum, n2 : NatNum ) : NatNum = {

    ( n1 ) match {

      case Z() => return n2;

      case Succ( n1 ) => return addNatNums( n1, Succ( n2 ) );

    }
  }
  /* --------------------------------------------------------------- */

  /* Multiply natural numbers */
  def multiplyNatNums( n1 : NatNum, n2 : NatNum ) : NatNum = {

    ( n1 ) match{

      case Z() => return Z();

      case Succ( n1 ) => {

        val s1 = multiplyNatNums( n1, n2 ); 
        return addNatNums( n2, s1 );              

      }
    }
  }
  /* --------------------------------------------------------------- */


  /**
   * Program entry point
   */
  def main( args: Array[ String ] ) : Unit = {

    /* Test minusOne() operation implemented with pattern matching */
    val zero : NatNum = Z();
    val one : NatNum = Succ( zero );
    val two : NatNum = Succ( one );
    val three : NatNum = Succ( two );
    val four : NatNum = Succ( three );
    /* --------------------------------------------------------------- */
    
    /* Ensure we are creating the intended chain of NatNum's */
    val four_again = Succ( Succ( Succ( Succ( Z() ) ) ) );
    assert( four == four_again);
    /* --------------------------------------------------------------- */

    /* Test minusOne() using pattern matching */
    val one_again = minusOne( minusOne( minusOne( four ) ) );
    assert( one == one_again );
    /* --------------------------------------------------------------- */

    /* Test minusOne() using pattern matching */
    try {
      minusOne( zero );
    }
    catch {
      case e: IllegalArgumentException =>
        println( "hello sir? sir? I caught another one..." );
    }
    /* --------------------------------------------------------------- */

    /* Add two natural numbers */
    val one_from_zero : NatNum = addNatNums( zero, one );
    val seven : NatNum = addNatNums( three, four );
    assert( one == one_from_zero)
    println( seven );
    /* --------------------------------------------------------------- */

    /* Multiply two natural numbers */
    val six_from_three : NatNum = multiplyNatNums( three, two );
    assert( addNatNums( four, two ) == six_from_three );
    println( six_from_three );
    /* --------------------------------------------------------------- */

  }
}