/** 
 * File: week3_ops_patternMatching.scala 
 * 
 * Working through the material on operations on inductively defined structures.
 */


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
/* --------------------------------------------------------------- */

/**
  * Grammar for a list of numbers:
  * 
  * Num = 0 | 1 | 2 | 3 . . .
  * NumList -> Nil | Const( Num, NumList )
  * 
  */
sealed trait NumList;

/* Inductive definition for List of Numbers */
case object Nil extends NumList;
case class Cons( num : Int, list : NumList ) extends NumList;
/* --------------------------------------------------------------- */


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

      case Z() => throw new IllegalArgumentException( "minuxOnePM cannot be called on Zero" );

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

  /* Get length of a list of numbers */
  def listLength( list : NumList ) : Int = {

    ( list ) match {

        case Nil => 0;

        case Cons( _, list ) => 1 + listLength( list );

    }
  }
  /* --------------------------------------------------------------- */

  /* Check if a list is in ascending order */
  def isAscendingOrder( list : NumList ) : Boolean = {

    ( list ) match {

        case Nil => return true;            // Empty list is in ascending order

        case Cons( _, Nil ) => return true; // Single element is in ascending order

        case Cons( j1, tl @ Cons( j2, _ ) ) => {

            if ( j1 <= j2 ) {

                return isAscendingOrder( tl );

            }
            else {

                return false;

            }
        }

        case _ => {
            assert( false );
            return false;
        }
    }
  }
  /* --------------------------------------------------------------- */

  /* Check if a list is in ascending order */
  def isAscendingOrder_2( list : NumList ) : Boolean = {

    ( list ) match {

      case Nil => return true;

      case Cons( _, Nil ) => return true;

      case Cons( j1, Cons( j2, _ ) ) if ( j1 > j2 ) => return false;

      case Cons( _, t1 ) => isAscendingOrder_2( t1 );

    }
  }
  /* --------------------------------------------------------------- */

  /**
   * Helper function appends 'element' to the end of list 'list'.
   * 
   * Time compleixty: O(n)
   * 
   * Note: Appending an element using this helper function requires
   *       recursing through every element in the list! From the front
   *       all the way to the back!
   */
  def appendHelper_1( list : NumList, element : Int ) : NumList = {

    ( list ) match {

      case Nil => {
        
        return Cons( element, Nil );  // Return list with element

      }

      /**
       * Recurse: call appendHelper_1 on the next element 't1'
       *          continue recursion, carrying through 'element'
       *          until: Nil where you will add 'element' to end
       */
      case Cons( j, t1 ) => { 

        return Cons( j, appendHelper_1( t1, element ) );

      }
    }
  }

  /** 
   * Reverse a list 
   * 
   * Time complexity: O(n^2)
   * 
   * Takes each element at the front of the list and 
   * use a helper function 'appendHelper_1' ( O(n) ) to append 
   * this element to the back of the list. 
   * 
   * Therefore, we must take each element from the front and append
   * it to the back. Reversing each element is O(n), but for each element 
   * reversed we must recurr through the entire list via appendHelper_1 
   * which is O(n) per element.
   * 
   * So total complexity = O(n^2)
   */
  def reverseList_1( list : NumList) : NumList = {

    ( list ) match {

      /* Empty list is already reversed */
      case Nil => return Nil;

      /* List with a single element is already reversed */
      case Cons( _, Nil ) => return list;

      /**
       * Recursively reverse the list using appendHelper 
       */
      case Cons( curr, subList ) => {

        // Recurse until: r1 = Cons( _, Nil )
        val r1 = reverseList_1( subList );
        
        /**
         * Recurr through every element, then append one element
         * at a time when r1 returns with: r1 = Cons( _, Nil ) 
         * from the base case */
        appendHelper_1( r1, curr );

      }
    }
  }
  /* --------------------------------------------------------------- */
  
  /** 
   * Reverse a list in O(n) time complexity
   */
  def reverseList_2( list : NumList, revList : NumList ) : NumList = {

    ( list ) match {

      case Nil => return revList;

      /**
       * n = Current number at the front of the list
       * tail = sublist, i.e. the rest of the list
       * 
       * e.g. list = Cons(4, Cons(3, Cons(2, Nil)));
       *      
       *      n = 4
       *      tail = Cons(3, Cons(2, Nil));
       */
      case Cons( n, tail ) => { 

        /**
         * Add the current number at the front of the list 'n' to the 
         * reversed list that is being constructed 
         * 
         * The reversed list being constructed is 'v1'
         * 
         * Recurse by passing the remaining tail (sublist) to be reversed
         * and 'v1' the reversed list being built.
         */
        val biggerRevList = Cons( n, revList );
        return reverseList_2( tail, biggerRevList );

      }
    }
  }

  def reverse( list : NumList ) : NumList = {

    return reverseList_2( list, Nil );

  }


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

    /* Get the length of a list of numbers */
    val list_1 : NumList = Cons( 7, Cons( 6, Cons( 5, Cons( 4, Cons( 3, Nil ) ) ) ) );
    val len_1 : Int = listLength( list_1 );
    assert( len_1 == 5 );
    /* --------------------------------------------------------------- */

    /* Check if a list is in ascending order */
    val list_2 : NumList = Cons( 1, Cons( 2, Cons( 3, Nil ) ) );
    println( isAscendingOrder( list_2 ) );
    /* --------------------------------------------------------------- */

    /* Check if a list is in ascending order */
    val list_3 : NumList = Cons( 1, Cons( 2, Cons( 3, Nil ) ) );
    println( isAscendingOrder_2( list_3 ) );
    /* --------------------------------------------------------------- */

    /* Reverse a list in n^2 time complexity */
    val list_4 : NumList = Cons( 1, Cons( 2, Cons( 3, Nil ) ) );
    println( reverseList_1( list_4 ) );
    /* --------------------------------------------------------------- */

    /* Reverse a list in n time complexity */
    val list_5 : NumList = Cons( 1, Cons( 2, Cons( 3, Nil ) ) );
    println( reverse( list_5 ) );
    /* --------------------------------------------------------------- */

  }
}