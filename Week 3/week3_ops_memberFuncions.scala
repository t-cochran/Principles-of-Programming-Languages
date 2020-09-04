/** 
 * File: week3_ops_memberFuncions.scala 
 * 
 * Working through the material on operations on inductively defined structures.
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

  def minusOne() : NatNum;                    // n1 - 1
  def addNatNums( num : NatNum ) : NatNum;    // n1 + n2
  def multNatNums( num : NatNum ) : NatNum;   // n1 x n2

}
/* --------------------------------------------------------------- */

/**
 * Inductive definition: Set of natural numbers
 * 
 * Case: Set of natural numbers where n = 0
 */
case class Z() extends NatNum {  // Number -> Z

  def minusOne() : NatNum = {    // Define the methods inherited from 'extends NatNum'

    throw ( new IllegalArgumentException( "minusOne cannot be called on Zero" ) );

  }

  def addNatNums( num : NatNum ) : NatNum = {

    return num;  // zero + num = num

  }

  def multNatNums( num : NatNum ) : NatNum = {
    
    return Z();  // zero * num = zero

  }
}
/* --------------------------------------------------------------- */

/**
 * Inductive definition: Set of natural numbers
 * 
 * Set of natural numbers where n = n + 1
 */
case class Succ( n : NatNum ) extends NatNum {  // Number -> Succ( Number )

  def minusOne() : NatNum = {  // Define the methods inherited from 'extends NatNum'

    return this.n;

  }

  def addNatNums( num : NatNum ) : NatNum = {

    return this.n.addNatNums( Succ( num ) );

  }

  def multNatNums( num : NatNum ) : NatNum = {

    val s1 = this.n.multNatNums( num );
    return s1.addNatNums( num );

  }

}
/* --------------------------------------------------------------- */

/**
  * Grammar for a list of numbers:
  * 
  * Num = 0 | 1 | 2 | 3 . . .
  * NumList -> Nil | Const( Num, NumList )
  * 
  * List the set of operations inherited from the class 'NumList_2'
  */
sealed trait NumList_2 {

    def listLength() : Int;

}
/* --------------------------------------------------------------- */

/**
 * Inductive definition: List of Numbers
 * 
 * List of numbers where List = Nil (empty)
 */
case object Nil_2 extends NumList_2 {

    def listLength() : Int = {

        return 0;

    }
}
/* --------------------------------------------------------------- */

/**
 * Inductive definition: List of Numbers
 * 
 * List of numbers where List != Nil
 */
case class Cons_2( num : Int, list : NumList_2 ) extends NumList_2 {

    def listLength(): Int = {

        return 1 + list.listLength();

    }

}
/* --------------------------------------------------------------- */


/**
 * Operations on inductive definitions using pattern matching
 */
object memberFunctions {


  /**
   * Program entry point
   */
  def main( args : Array[ String ] ) : Unit = {

    /* Test adding one to 'NatNum' starting with Z() */
    val zero : NatNum = Z();
    val one : NatNum = Succ( zero );
    val two : NatNum = Succ( one );
    val three : NatNum = Succ( two );
    val four : NatNum = Succ( three );
    /* --------------------------------------------------------------- */

    /* Test manually adding +1 to each NatNum */
    val four_again : NatNum = Succ( Succ( Succ( Succ( Z() ) ) ) );
    ( four == four_again ) match {

      case true => println( "four == four_again" );
      case false => println( "four != four_again" );

    }
    /* --------------------------------------------------------------- */

    /* Test the 'minusOne()' operation */
    val two_again : NatNum = four.minusOne().minusOne();
    ( two == two_again ) match {

      case true => println( "two == two_again" );
      case false => println( "two != two_again" );

    }
    /* --------------------------------------------------------------- */
    
    /* Test the 'minusOne()' operation error handling */
    try {
      zero.minusOne();  // Z() cannot be negative
    }
    catch {
      case e: IllegalArgumentException => 
        println( "hello sir? sir? Excuse me sir... You dropped this error" );
    }
    /* --------------------------------------------------------------- */

    /* Test the 'addNatNums()' operation */
    val one_from_zero : NatNum = zero.addNatNums( one );
    assert( one == one_from_zero );
    val seven : NatNum = four.addNatNums( three );
    println( seven );
    /* --------------------------------------------------------------- */

    /* Test the 'multNatNums()' operation */
    val six : NatNum = three.multNatNums( two );
    println( s"Six: $six" );
    /* --------------------------------------------------------------- */

    /* Get the length of a list of numbers */
    val list_2 : NumList_2 = Cons_2( 3, Cons_2( 2, Cons_2( 1, Nil_2 ) ) );
    assert( list_2.listLength() == 3 );
    /* --------------------------------------------------------------- */

  }
}