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
case class Z() extends NatNum;
case class Succ( n : NatNum ) extends NatNum;
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
  * Grammar for a binary tree of numbers:
  * 
  * Num = 0 | 1 | 2 | 3 . . .
  * NumTree -> Leaf | Node( Num, NumTree, NumTree ) 
  */
sealed trait NumTree;

/* Inductive definition for a binary tree of numbers */
case object Leaf extends NumTree;
case class Node( n : Int, left : NumTree, right : NumTree ) extends NumTree;
/* --------------------------------------------------------------- */

/**
  * Grammar for arithmetic expressions: 
  * 
  * Double -> -2 | -1 | 0 | 1 | 2 . . .
  * Identifier -> [a-zA-Z][a-zA-Z0-9_]
  * Expr -> Const( Double )
  *     |   Ident( Identifier )
  *     |   Plus( Expr, Expr )
  *     |   Minus( Expr, Expr )
  *     |   Mult( Expr, Expr )
  *     |   Div( Expr, Expr )
  *     |   Log( Expr )
  *     |   Exp( Expr )
  *     |   Sine( Expr )
  *     |   Cosine( Expr )
  */
sealed trait Expr;

/* Inductive definition for arithmetic expressions */
case class Const( f: Double ) extends Expr 
case class Ident( s: String ) extends Expr
case class Plus(  eList: List[Expr] ) extends Expr
case class Minus( e1: Expr, eList: List[Expr] ) extends Expr
case class Mult( eList: List[Expr] ) extends Expr
case class Div( e1: Expr, e2: Expr ) extends Expr
case class Log( e: Expr ) extends Expr
case class Exp( e: Expr ) extends Expr
case class Sine( e: Expr ) extends Expr
case class Cosine( e: Expr ) extends Expr

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
            println( s"Printing tl: $tl" );
            println( tl );
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
  /* --------------------------------------------------------------- */

  /* Check if a BST follows the BST property */
  def isBST_helper( tree : NumTree, left : Int, right : Int ) : Boolean = {

    ( tree ) match {

      case Leaf => return true;  // A single leaf does follow BST property

      case Node( self, lChild, rChild ) if (left <= self && self <= right) => {

        return isBST_helper( lChild, left, self ) && 
               isBST_helper( rChild, self, right );

      }

      case _ => false;

    }
  }

  def isBST( tree : NumTree ) : Boolean = {

    return isBST_helper( tree, Int.MinValue, Int.MaxValue );

  }
  /* --------------------------------------------------------------- */

  /* Return the set of variables (as strings) used in an expression */
  def collectAllVariables( expression : Expr ): Set[ String ] = {

    /* Use foldLeft to collect variables from expressions in an expression list */
    def helperFunction( expList: List[ Expr ], startValue: Set[ String ] ): Set[ String ] = {

      expList.foldLeft( startValue ) {  // Pass each expression in expList

        case ( setSoFar: Set[ String ], newExpr : Expr ) => { 

          val v = collectAllVariables( newExpr );
          setSoFar union v;
        
        }
      }
    }
    
    /* Match the expression found */
    ( expression ) match {

        /* Simple cases */
        case Const( f ) => Set()    // Return an empty set
        case Ident( s ) => Set( s ) // Return a set with identifier string
        
        /* Call the helper function if an expression contains a list of expressions */
        case Plus( expList ) => helperFunction( expList, Set() )
        case Mult( expList ) => helperFunction( expList, Set() )
        case Minus(e1, expList) => {

            val e1Set = collectAllVariables(e1)
            helperFunction(expList, e1Set) 

        }

        /* Recurse through sub-expressions and collect variable strings */
        case Div( e1, e2 ) => {

            ( collectAllVariables( e1 ) ) union ( collectAllVariables( e2 ) )

        }
        case Log( e1 ) => collectAllVariables( e1 )
        case Sine( e1 ) => collectAllVariables( e1 )
        case Cosine( e1 ) => collectAllVariables( e1 )
        case Exp( e1 ) => collectAllVariables( e1 )

        /* Catch all case */
        case _ => { 

          assert(false); return Set()  
        
        }
    }
  }
  /* --------------------------------------------------------------- */

  /**
   * Eval function
   * 
   * Take an environment that maps variable identity strings to values.
   * Use the environment mapping to evaluate arithmetic expressions.
   */
  def evalExpression( expr : Expr, env : Map[ String, Double ] ) : Double = {

    /* Use pattern matching to evaluate each type of arithmetic expression */
    ( expr ) match {

      case Const( num ) => return num;  // Simply return constants

      case Ident( str ) => {  // Return the number mapped to the identity
        if ( env.contains( str ) ) {
          return env( str );
        }
        else {
          throw new IllegalArgumentException( s"No mapping for $str" );
        }
      }

      /* Expression contains a list of sub-expressions */
      case Plus( exprList ) => {  
        return ( exprList map { evalExpression( _, env ) } ).sum
      }
      case Minus( exp, exprList ) => {
        return ( evalExpression( exp, env ) ) - 
               ( ( exprList map { evalExpression( _, env ) } ).sum );
      }
      case Mult( exprList ) => {
        return ( exprList map { evalExpression( _, env ) } ).product
      }

      /* Evaluate single expressions using the math library */
      case Div( exp1, exp2 ) => return ( evalExpression( exp1, env ) / 
                                         evalExpression( exp2, env ) );
      case Log( exp ) => return math.log( evalExpression( exp, env ) );
      case Exp( exp ) => return math.exp( evalExpression( exp, env ) );
      case Sine( exp ) => return math.sin( evalExpression( exp, env ) );
      case Cosine( exp ) => return math.cos( evalExpression( exp, env ) );
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

    /* Check if binary trees have the BST property */
    val tree1 = Node(1, 
                  Node(2, 
                    Node(3, Leaf, Leaf), 
                    Node(2, Leaf, Leaf)
                  ), 
                  Node(4, Leaf, Leaf)
                )
    val tree2 = Node(6, 
                  Node(2, 
                    Node(1, Leaf, Leaf), 
                    Leaf
                  ), 
                  Node(10, 
                    Node(7, Leaf, Leaf), 
                    Leaf
                  )
                )
    println( isBST( tree1 ) );
    println( isBST( tree2 ) );
    /* --------------------------------------------------------------- */

    /** 
     * Create an expression: 
     *  
     * cos(x) + sin(y) + e^{x - y - z}
     * 
     * Get all variables used in the expression.
     */
    val x = Ident( "x" );
    val y = Ident( "y" );
    val z = Ident( "z" );

    val expr1 = Plus(List(Cosine(x), Sine(y), Exp(Minus(x, List(y,z)))))
    val s1 = collectAllVariables( expr1 );
    println( s1 );
    /* --------------------------------------------------------------- */

    /**
     * Evaluate expressions using a given environment
     */
    val env : Map[ String, Double ] = Map( "x" -> 2.0, "y" -> 1.5, "z" -> 2.8 );

    /* expr1 =  cos(x) + sin(y) + e^{ x - y - z } */
    val eval = evalExpression( expr1, env );
    println( eval );


  }
}