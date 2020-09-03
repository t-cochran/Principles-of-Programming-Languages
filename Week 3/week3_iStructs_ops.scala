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
  def addNatNums( num : NatNum) : NatNum;

}

/**
 * Inductive definition: Set of natural numbers
 * 
 * Case: List of numbers where n = 0
 */
case class Z() extends NatNum {  // Number -> Z

  def minusOne() : NatNum = {    // Define the methods inherited from 'extends NatNum'

    throw ( new IllegalArgumentException( "minusOne cannot be called on Zero" ) );

  }

  def addNatNums( num : NatNum ) : NatNum = {

    return num;  // zero + num = num

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

  def addNatNums( num : NatNum ) : NatNum = {

    return this.n.addNatNums( Succ( num ) );

  }

}
/* --------------------------------------------------------------- */

/* Member Functions ---------------------------------------------- */
object memberFunctions {

  def main( args : Array[ String ] ) : Unit = {

    /* Test adding one to 'NatNum' starting with Z() */
    val zero : NatNum = Z();
    val one : NatNum = Succ( zero );
    val two : NatNum = Succ( one );
    val three : NatNum = Succ( two );
    val four : NatNum = Succ( three );

    /* Test manually adding +1 to each NatNum */
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

    /* Test the 'addNatNums()' operation */
    val one_from_zero : NatNum = zero.addNatNums( one );
    assert( one == one_from_zero );
    val seven : NatNum = four.addNatNums( three );
    println( seven );

    println( four.n );

  }
}

/* Pattern Matching ---------------------------------------------- */
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
      case Succ( n ) => return n;

    }
  }

  /* Add natural numbers */
  def addNatNums( n1 : NatNum, n2 : NatNum ) : NatNum = {

    ( n1 ) match {

      case Z() => return n2;

      case Succ( n ) => return addNatNums( n, Succ( n2 ) );

    }
  }

  def main( args: Array[ String ] ) : Unit = {

    /* Test minusOne() operation implemented with pattern matching */
    val zero : NatNum = Z();
    val one : NatNum = Succ( zero );
    val two : NatNum = Succ( one );
    val three : NatNum = Succ( two );
    val four : NatNum = Succ( three );
    
    /* Ensure we are creating the intended chain of NatNum's */
    val four_again = Succ( Succ( Succ( Succ( Z() ) ) ) );
    assert( four == four_again);

    /* Test minusOne() using pattern matching */
    val one_again = minusOne( minusOne( minusOne( four ) ) );
    assert( one == one_again );

    /* Test minusOne() using pattern matching */
    try {
      minusOne( zero );
    }
    catch {
      case e: IllegalArgumentException =>
        println( "hello sir? sir? I caught another one..." );
    }

    /* Add two natural numbers */
    val one_from_zero : NatNum = addNatNums( zero, one );
    val seven : NatNum = addNatNums( three, four );
    assert( one == one_from_zero)
    println( seven );

  }

}