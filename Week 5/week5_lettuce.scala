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

  }

}
