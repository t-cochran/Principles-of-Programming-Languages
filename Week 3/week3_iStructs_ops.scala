/** 
 * File: week3_iStructs_ops.scala 
 * 
 * Working through the material on operations on inductively defined structures.
 * A bit of scala pattern matching as well.
 * 
 */

/**
  * Grammar for natural numbers:
  *  
  * Number -> Z 
  *       | Succ( Number )
  * 
  * { ∅ / 0 } U { { n }/n+1 | n ∈ ℕ } 
  * 
  * List the set of operations inherited from the class 'NatNum'
  */
sealed trait NatNum {

  def minusOne() : NatNum;

}


object structOperations {

/* --------------------------------------------------------------- */
  /**
   * Inductive definition: Set of natural numbers
   * 
   * Case: List of numbers where n = 0
   */
  case class Z() extends NatNum {  // Number -> Z

    def minusOne() : NatNum = {    // Define the methods inherited from 'extends NatNum'

      throw ( new IllegalArgumentException( "minusOne cannot be called on Zero" ) );

    }
  }

  /**
   * Inductive definition: Set of natural numbers
   * 
   * List of numbers where n = n + 1
   */
  case class Succ( n : NatNum ) extends NatNum {  // Number -> Succ( Number )

    def minusOne() : NatNum = {  // Define the methods inherited from 'extends NatNum'

      return this.n;

    }

  }
/* --------------------------------------------------------------- */

  def main( args : Array[ String ] ) : Unit = {

    /* Test adding one two 'NatNum' starting with Z() */
    val zero : NatNum = Z();
    val one : NatNum = Succ( zero );
    val two : NatNum = Succ( one );
    val three : NatNum = Succ( two );
    val four : NatNum = Succ( three );

    /* Test the operation of manually adding +1 to each NatNum */
    val four_again : NatNum = Succ( Succ( Succ( Succ( Z() ) ) ) );
    ( four == four_again ) match {

      case true => println( "four == four_again" );
      case false => println( "four != four_again" );

    }

    /* Test the 'minusOne()' operation */
    val two_again : NatNum = four.minusOne().minusOne();
    ( two == two_again ) match {

      case true => println( "two == two_again" );
      case false => println( "two != two_again" );

    }
    
    /* Test the 'minusOne()' operation error handling */
    try {
      zero.minusOne();  // Z() cannot be negative
    }
    catch {
      case e: IllegalArgumentException => 
        println( "hello sir? sir? Excuse me sir... You dropped this error" );
    }


  }

}