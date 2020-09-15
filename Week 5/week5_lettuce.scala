/** 
 * File: week5_lettuce.scala
 * 
 * Working through the material on the Let language
 */

/**
 * Grammar for Lettuce
 */
sealed trait Program
sealed trait Expr

case class TopLevel( e : Expr ) extends Program
case class Const( v : Double ) extends Expr
case object True extends Expr
case object False extends Expr
case class Ident( s : String ) extends Expr

case class Plus( e1: Expr, e2: Expr ) extends Expr
case class Minus( e1: Expr, e2: Expr ) extends Expr
case class Mult( e1: Expr, e2: Expr ) extends Expr
case class Div( e1: Expr, e2: Expr ) extends Expr
case class Log( e: Expr ) extends Expr 
case class Exp( e: Expr ) extends Expr
case class Sine( e: Expr ) extends Expr
case class Cosine( e: Expr ) extends Expr

case class Geq( e1: Expr, e2:Expr ) extends Expr
case class Eq( e1: Expr, e2: Expr ) extends Expr
case class And( e1: Expr, e2: Expr ) extends Expr
case class Or( e1: Expr, e2: Expr ) extends Expr
case class Not( e: Expr ) extends Expr
case class IfThenElse( e: Expr, eIf: Expr, eElse: Expr ) extends Expr

case class Let( s: String, defExpr: Expr, bodyExpr: Expr ) extends Expr
case class FunDef( param: String, bodyExpr: Expr ) extends Expr
case class FunCall( funCalled: Expr, argExpr: Expr ) extends Expr


object lettuceScratch {

  /**
   * Create a curried function 
   * 
   * First call: returns a function with 'x' passed into the definition
   * Second call: calls the returned function with 'y' passed
   */
  def curriedFunction( x : Int )( y : Int ) = x + y;
  /* --------------------------------------------------------------- */


  /**
   * Create a curried function:
   * 
   * First call: returns a function 'f' that takes a user defined function
   *             with x * y as the parameter. ex: mult( 5, 10 ) will return
   *             a function that will take a user defined function and pass
   *             (50) to it.
   * Second call: returns the value 'Int' from a user function passed to the
   *              curried function. The result of the first call will be the
   *              parameter passed to the user function.
   * 
   * Again:
   *      First curied () means we pass x and y, and then the result x * y
   *      is the parameter passed to the function 'f' passed to the second 
   *      curried (). The second curried () is a function 'f' which is 
   *      user defined, but must take an int and return an int ( Int => Int ).
   *      The result: pass a function that takes an int, returns an int, the 
   *      function passed gets parameter ( x * y ) from the first call.
   */
  def mult( x : Int, y : Int )( f : Int => Int ) = { f( x * y ) };
  /* --------------------------------------------------------------- */


  /**
   * Catch errors while parsing ( static type checking ) our Lettuce grammar
   * 
   * Create an 'undeclared identifier' error if a variable is not declared
   * before use. E.g. Let x = 10 in ...
   * 
   * e: expression
   * s: set of in-scope declared expressions
   */
  def declared( e : Expr, s : Set[ String ] ) : Boolean = {

    /* Pattern match the expression 'e' for a given operation */
    ( e ) match {

      /**
       * Base case:
       * 
       * Const: Constants require no declaration before use. They cannot
       *        be an undeclared ientifier by default. So, return true.
       * 
       * Booleans: Booleans, like constants, require no declaration before
       *           use. They cannot be undeclared, so return true.
       */
      case Const( _ ) => true;
      case True => true;
      case False => true;

      /**
       * Filter: In any given op, what you're left with are nested constants,
       *         booleans, or identifiers. Since constants and booleans 
       *         don't need to be declared, we need only check if identifiers
       *         have been declared prior to use.
       * 
       * How to check of an Ident( x ) has been declared before use:
       *    
       *                  x ∈ s
       *        ----------------------------
       *          declared( Ident( x ), s )
       * 
       * In other words:
       *    If identifier 'x' exists in the set of in-scope declared expressions,
       *    then Ident( x ) has been declared prior ot use.
       */
      case Ident( x ) => {
        if ( s contains x ) { true }
        else { println( s"Error: Undeclared identifier $x" ); false }
      }

      /**
       * Pattern match a given operation (binary or unary), then use recursion
       * to strip off nested operations until we reach either the base case
       * (constants or booleans == true) or an Identifier, which we check.
       */

      /**
       * Binary operations
       * 
       *   declared( e1, s ), declared ( e2, s ), T ∈ { Plus, Minus, Mult .. }
       *  ---------------------------------------------------------------------
       *                     declared( T( e1, e2 ), s )
       */
      case Plus( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Minus( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Mult( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Div( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Geq( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Eq( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case And( e1, e2 ) => declared( e1, s ) && declared( e2, s );
      case Or( e1, e2 ) => declared( e1, s ) && declared( e2, s );

      /**
       * Unary operations
       * 
       *   declared( e1, s ), T ∈ { Log, Exp, Sine .. }
       *  ----------------------------------------------
       *            declared( T( e1 ), s )
       */
      case Log( e ) => declared( e, s ); 
      case Exp( e ) => declared( e, s ); 
      case Sine( e ) => declared( e, s ); 
      case Cosine( e ) => declared( e, s ); 
      case Not( e ) => declared( e, s ); 

      /* IfThenElse: Recurse through all three expressions */
      case IfThenElse( e, eIf, eElse ) => {
        declared( e, s ) && declared( eIf, s ) && declared( eElse, s );
      }

      /** 
       * Let: 
       * 
       * Recurse through  defining expression, body expression, and
       * append the symbol to the set of in-scope declared expressions.
       */
      case Let( symbolStr, defExpr, bodyExpr ) => {
        declared( defExpr, s ) && declared( bodyExpr, s + symbolStr )
      }

      /** 
       * Function definition and function call:
       * 
       * Recurse through function body expressions, the function call and
       * function arguments. Append function parameter string to the set
       * of in-scope declared expressions.
       */
      case FunDef( paramStr, bodyExpr ) => declared( bodyExpr, s + paramStr );
      case FunCall( funcCall, funcArgExpr ) => {
        declared( funcCall, s ) && declared( funcArgExpr, s );
      }

    }

  }
  /* --------------------------------------------------------------- */
  

  /**
   * Helper for 'declared' function:
   * 
   * Pass program abstract syntax to 'p', it will pass 'p' to 'declared'
   * along with an empty set for declared expressions.
   */
  def checkProgram( p : Program ) : Boolean = {

    ( p ) match {

      case TopLevel( e ) => declared( e, Set() );

    }

  }
  /* --------------------------------------------------------------- */


  def main( args : Array[ String ] ) : Unit = {

    /* Test the curried function */
    val firstCall : Int => Int = curriedFunction( 100 );  // ret: f(y) = 100 + y
    val secondCall : Int = firstCall( 50 );               // ret: f( 50 ) = 100 + 50 = 150
    println( secondCall );

    /* Test another curried function */
    val myFunction : Int => Int = { x => 10 * x };  // define function: Int => Int
    val firstC  = mult( 10, 50 ) {  myFunction }    // x:10, y:50 ret 50 passed to Int => Int
    println( firstC );                              // ret: f( 50 ) = 10 * 50 = 5000

    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     * 
     * let f = function( x ) x * 10 in        // function definition
     *    11 + f( 10 )                        // function call
     */
    val program_1 = TopLevel( 
                      Let( 
                        "f", FunDef( "x", Mult( Ident( "x"), Const( 10 ) ) ), 
                        Plus( Const( 11 ), FunCall( Ident( "f" ), Const( 10 ) ) ) 
                      ) 
                    )
    println( program_1 );

    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     *
     * let x = 10 + 15 in
     *  let y = x >= 25 in
     *    if (y)
     *    then x
     *    else x - 35
     */
    val program_2 = TopLevel(
                      Let( 
                        "x", Plus( Const( 10 ), Const( 15 ) ),    // x = 10 + 15
                        Let( 
                          "y", Geq( Ident( "x" ), Const( 25 ) ),  // y >= 25
                          IfThenElse( 
                            Ident( "y" ),                         // If
                            Ident( "x" ),                         // then
                            Minus( Ident( "x" ), Const( 35 ) )    // else
                          )
                        )
                      )
                    )
    println( program_2 );

    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     *
     * let x = y + z in 
     *  x * y
     */
    val program_3 = TopLevel(
                      Let( 
                        "x", Plus( Ident( "y" ), Ident( "z" ) ), 
                        Mult( Ident( "x" ), Ident( "y" ) ) 
                      )
                    );
    println( program_3 );

    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     *
     * let x = x in 
     *  x * y
     */
    val program_4 = TopLevel(
                      Let( 
                        "x", Ident( "x" ), 
                        Mult( Ident( "x" ), Ident( "y" ) ) 
                      )
                    );
    println( program_4 );

    /**
     * Test checking whether variables are declared before use
     */
    println( s"Checking program_1 declared variables: ${ checkProgram( program_1 ) }" );
    println( s"Checking program_2 declared variables: ${ checkProgram( program_2 ) }" );
    println( s"Checking program_3 declared variables: ${ checkProgram( program_3 ) }" );  // Error
    println( s"Checking program_4 declared variables: ${ checkProgram( program_4 ) }" );  // Error
  }

}
