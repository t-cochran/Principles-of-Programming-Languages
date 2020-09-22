/** 
 * File: week6_lettuceFunctions.scala
 * 
 * Working through the material on the Let language
 */

/**
 * Grammar for Lettuce types and expressions
 * 
 * Program  ->  TopLevel( Expr )
 * 
 * Expr   ->   Const( Number )
 *        |    True
 *        |    False
 *        |    Ident( String )  
 *        |    Plus( Expr, Expr )
 *        |    Minus( Expr, Expr )
 *        |    Mult( Expr, Expr )
 *        |    Div( Expr, Expr )
 *        |    Geq( Expr, Expr )
 *        |    Eq( Expr, Expr )
 *        |    And( Expr, Expr )
 *        |    Or( Expr, Expr )
 *        |    Log( Expr )
 *        |    Exp( Expr )
 *        |    Sine( Expr )
 *        |    Cosine( Expr )
 *        |    Not( Expr )
 *        |    IfThenElse( Expr, Expr, Expr )
 *        |    Let( String, Expr, Expr )
 *        |    FunDef( String, Expr )
 *        |    FunCall( Expr, Expr )
 * 
 * Double -> -2 | -1 | 0 | 1 | 2 ...
 * String -> [a-zA-Z0-9_-]+
 * Boolean -> true | false
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
case class Geq( e1: Expr, e2: Expr ) extends Expr
case class Eq( e1: Expr, e2: Expr ) extends Expr
case class And( e1: Expr, e2: Expr ) extends Expr
case class Or( e1: Expr, e2: Expr ) extends Expr

case class Log( e: Expr ) extends Expr 
case class Exp( e: Expr ) extends Expr
case class Sine( e: Expr ) extends Expr
case class Cosine( e: Expr ) extends Expr
case class Not( e: Expr ) extends Expr

case class IfThenElse( e: Expr, eIf: Expr, eElse: Expr ) extends Expr

case class Let( s: String, defExpr: Expr, bodyExpr: Expr ) extends Expr
case class FunDef( param: String, bodyExpr: Expr ) extends Expr
case class FunCall( funCalled: Expr, argExpr: Expr ) extends Expr
/* --------------------------------------------------------------- */

/**
 * Grammar for Lettuce values that are evaluated
 *
 * Value -> Closure( String, Expr, Map[String, Value] )
 *       |  NumValue( Double )
 *       |  BoolValue( Boolean )
 *       |  ErrorValue
 * 
 * Double -> -2 | -1 | 0 | 1 | 2 . . .
 * Boolean -> true | false
 * String -> [a-zA-Z0-9_-]+
 */
sealed trait Value
case class Closure( p : String, e : Expr, pi : Map[ String, Value ] ) extends Value;
case class NumValue( d : Double ) extends Value
case class BoolValue( b : Boolean ) extends Value
case object ErrorValue extends Value
/* --------------------------------------------------------------- */

/**
 * Grammar for Lettuce event class used to demo callbacks
 * 
 * Event -> KeyPress( Int )
 *        | MouseClick( Int, Int )
 *        | PrintStatistics()
 */
sealed trait Event;
case class KeyPress( key : Int ) extends Event;
case class MouseClick( x : Int, y : Int ) extends Event;
case object PrintStatistics extends Event;
/* --------------------------------------------------------------- */


/**
 * Lettuce eval function that takes a Lettuce program written in
 * abstract syntax, and evaluates the program to return a result
 */
object lettuceEval {

  /**
   * Helper functions:
   * 
   * Convert a Lettuce value from the eval function to a double, 
   * boolean, or closure
   */
  def valConvert( v : Value ) : Double = {

    ( v ) match {

      case NumValue( d ) => d;
      case _ => throw new IllegalArgumentException( s"Error converting value: $v" );

    }

  }

  def boolConvert( v : Value ) : Boolean = {

    ( v ) match {

      case BoolValue( b ) => b;
      case _ => throw new IllegalArgumentException( s"Error converting value: $v" );

    }

  }

  def closureConvert( v : Value ) : Closure = {

    ( v ) match {

      case Closure( p, e, pi ) => Closure( p, e, pi );
      case _ => throw new IllegalArgumentException( s"Error converting value: $v" );

    }

  }
  /* --------------------------------------------------------------- */

  /**
   * Evaluate a lettuce expression for a given environment
   */
  def eval( e : Expr, env : Map[ String, Value ] ) : Value = {

    /**
     * Methods that get values from either a binary, unary or boolean operation,
     * then perform a given function 'f' on those values, and return the result.
     * 
     * Curried -- pass in expressions 1 and 2 and recursively evaluate them
     * to their NumValue, then convert the NumValue to Double's.
     * 
     * Pass expressions 1 and 2 to whatever function 'f' you want.
     */
    def binaryNumOp( e1 : Expr, e2 : Expr )( f : ( Double, Double ) => Double ) = {
      val val_1 : Double = valConvert( eval( e1, env ) );  // Eval expr 1
      val val_2 : Double = valConvert( eval( e2, env ) );  // Eval expr 2
      val funcResult : Double = f( val_1, val_2 );         // Pass expressions to 'f'
      NumValue( funcResult );
    }
    def boolOp( e1 : Expr, e2 : Expr )( f : ( Double, Double ) => Boolean ) = {
      val val_1 : Double = valConvert( eval( e1, env ) );  // Eval expr 1
      val val_2 : Double = valConvert( eval( e2, env ) );  // Eval expr 2
      val funcResult : Boolean = f( val_1, val_2 );        // Pass expressions to 'f'
      BoolValue( funcResult );                        
    }
    def unaryOp( e1 : Expr )( f : ( Double ) => Double ) ={
      val val_1 : Double = valConvert( eval( e1, env ) );
      val funcResult : Double = f( val_1 );
      NumValue( funcResult );
    }

    /**
     * Pattern match the expression passed
     */
    ( e ) match {

      /**
       * Base cases:
       * 
       * Constants and booleans do not require mappings in env; they are returned.
       * Identifiers return the mapped value from the environment.
       */
      case Const( d ) => NumValue( d );
      case True => BoolValue( true );
      case False => BoolValue( false );
      case Ident( d ) => {
        if ( env contains d ) {
          env( d );
        }
        else {
          throw new IllegalArgumentException( s"Error: Ident( $d ) not found in env" );
        }
      }

      /**
       * Evaluate binary expressions
       * 
       * Use the method 'binaryOp' or 'boolOp' which evaluate e1 and e2 via recursion, 
       * then call the anonymous function on the values of each expression.
       */
      case Plus( e1, e2 ) => binaryNumOp( e1, e2 ) ( _ + _ )
      case Minus( e1, e2 ) => binaryNumOp( e1, e2 ) ( _ - _ )
      case Mult( e1, e2 ) => binaryNumOp( e1, e2 ) ( _ * _ )
      case Geq( e1, e2 ) => boolOp( e1, e2 ) ( _ >= _ )
      case Eq( e1, e2 ) => boolOp( e1, e2 ) ( _ == _ )
      case Div( e1, e2 ) => binaryNumOp( e1, e2 ) {
        case ( _, 0.0 ) => throw new IllegalArgumentException( s"Error: div by zero" );
        case ( num, denom ) => num / denom;
      }
      case And( e1, e2 ) => {
        val val_1 : Value = eval( e1, env ); // evaluate the first expression
        ( val_1 ) match {
          case BoolValue( false ) => BoolValue( false );  // short circuit
          case BoolValue( true ) => {
            val val_2 : Value = eval( e2, env );
            ( val_2 ) match {
              case BoolValue( _ ) => val_2;
              case _ => throw new IllegalArgumentException( s"Error: And() expr not boolean" );
            }
          }
          case _ => throw new IllegalArgumentException( s"Error: And() expr not boolean" );
        }
      }
      case Or( e1, e2 ) => {
        val val_1 : Value = eval( e1, env );
        ( val_1 ) match {
          case BoolValue( true ) => BoolValue( true );  // short circuit
          case BoolValue( false ) => {
            val val_2 : Value = eval( e2, env );
            ( val_2 ) match {
              case BoolValue( _ ) => val_2; 
              case _ => throw new IllegalArgumentException( s"Error: Or() expr not boolean" );
            }
          }
          case _ => throw new IllegalArgumentException( s"Error: Or() expr not boolean" );
        }
      }

      /**
       * Evaluate unary expressions
       * 
       * Use the method 'unaryOp'. This will evaluate the NumValue of 
       * e1 and e2 via recursion, then call the anonymous function on 
       * the values of each expression.
       */
      case Log( e ) => unaryOp( e ) {
        case v if v > 0.0 => math.log( v )
        case v => throw new IllegalArgumentException( 
          s"Log of a negative number ${ e } evaluates to ${ v }!"
        )
      }
      case Exp( e ) => unaryOp( e ) ( math.exp )
      case Sine( e ) => unaryOp( e ) ( math.sin )
      case Cosine( e ) => unaryOp( e ) ( math.cos )
      case Not( e ) => {
        val val_1 : Value = eval( e, env )
        ( val_1 ) match {
          case BoolValue( b ) => BoolValue( !b )
          case _ => throw new IllegalArgumentException( 
            s"Not of a non-boolean expr: ${ e } which evaluated to ${ val_1 }" 
          )
        }
      }
      
      /**
       * Evaluate If-Then-Else expressions
       */
      case IfThenElse( e1, e2, e3 ) => {
        val val_1 : Value = eval( e1, env )
        ( val_1 ) match {
          case BoolValue( true ) => eval( e2, env )
          case BoolValue( false ) => eval( e3, env )
          case _ => throw new IllegalArgumentException(
            s"If-then-else condition expr: ${ e1 } is non-boolean -- evaluates to ${ val_1 }"
          )
        }
      }

      /**
       * Evaluate Let expressions
       */
      case Let( x, e1, e2 ) => {
        val val_1 = eval( e1, env )      // eval e1
        val env_2 = env + ( x -> val_1 ) // create a new extended env
        eval( e2, env_2 )                // eval e2 under that.
      }
  
      /**
       * Evaluate function definitions
       */
      case FunDef( param, body ) => {
        Closure( param, body, env );  // Return a closure with its env at define time
      }
      case FunCall( funcIdent, funcArg ) => { 
        val funcDefEval = eval( funcIdent, env );
        val funcArgEval = eval( funcArg, env );
        ( funcDefEval ) match {
          case Closure( funcParam, funcBody, closedFuncEnv ) => {
            // Extend the function environment to include the evaluated argument
            val extendedEnv = closedFuncEnv + ( funcParam -> funcArgEval )
            // Evaluate the function body under the extended environment that
            // contains the evaluated function argument
            eval( funcBody, extendedEnv );
          }
          case _ => throw new IllegalArgumentException(
            s"Function call error: ${ funcIdent } does not eval to a closure"
          )
        }
      }
    }
  }

  /**
   * Pass expressions from the TopLevel program to the eval function with 
   * an empty environment mapping. 
   */
  def evalProgram( p : Program ) = {
    val m : Map[ String, Value ] = Map[ String, Value ]();
    ( p ) match {
      case TopLevel( e ) => {
        try {
          eval( e, m );
        } catch {
          case e: IllegalArgumentException => {
            println( s"Error: $e" );
            ErrorValue
          }
        }
      }
    }
  }
  /* --------------------------------------------------------------- */

  def main( args : Array[ String ] ) : Unit = {
    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     *
     * let x = 100 in
     *  let y = 500 - x * 2 in
     *    x * y
     * 
     * Expected result: 30000
     */
    val program_1 = TopLevel(
                      Let( 
                        "x", Const( 100 ), 
                        Let ( 
                          "y", Minus( Const( 500 ), Mult( Ident( "x" ), Const( 2 ) ) ), 
                          Mult( Ident( "x" ), Ident( "y" ))
                        )
                      )
                    );
    println( program_1 );

    /**
     * Test Lettuce Grammar: Concrete to abstract syntax
     *
     * let x = 10 + 15 in
     *  let y = x >= 25 in
     *    if (y)
     *    then x
     *    else x - 35
     * 
     * Expected result: 25
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
     * let x = 5 in
     *  let y = 5 in
     *    let z = x / y in 
     *      if ( z == 1 )
     *      then 100
     *      else -1
     * Expected result: 100
     */
    val program_3 = TopLevel(
                      Let(
                        "x", Const( 5 ), 
                        Let(
                          "y", Const( 5 ),
                          Let(
                            "z", Div( Ident( "x" ), Ident( "y" ) ), 
                            IfThenElse( 
                              Eq( Ident( "z" ), Const( 1 ) ), 
                              Const( 100 ), 
                              Const( -1 ) )
                          )
                        )
                      )
                    )
    println( program_3 );

    /**
     * Evaluate program_1, program_2, and program_3 using the eval function
     */
    val ret_1 : Value = evalProgram( program_1 );
    val ret_2 : Value = evalProgram( program_2 );
    val ret_3 : Value = evalProgram( program_3 );

    println( 
      s"""
      program_1 evaluated: ${ valConvert( ret_1 ) } 
      program_2 evaluated: ${ valConvert( ret_2 ) }
      program_3 evaluated: ${ valConvert( ret_3 ) }
      """ 
    );
    /* --------------------------------------------------------------- */

    /**
     * Function definition example 1
     *
     * let square = function(x) x * x in
     *     square( 10 )
     */
    val program_4 = TopLevel(
                      Let( "square", FunDef( "x", Mult( Ident( "x" ), Ident( "x" ) ) ), 
                            FunCall( Ident( "square" ), Const( 10 ) ) 
                      ) 
                    );
    /*-- Again but with parts --*/
    val x = Ident( "x" );
    val square = Ident( "square" );
    val func_def_sqr = FunDef( "x", Mult( x, x ) );
    val func_call_sqr = FunCall( square, Const( 10 ) );
    val program_4_again = TopLevel( Let( "square", func_def_sqr, func_call_sqr ) );
    assert( program_4 == program_4_again );

    /**
     * Function definition example 2
     *
     * let x = 10 in
     * let y = 15 in
     * let sq1 = function (x)
     *              function (y)
     *                  x + y * y
     *           in
     *      sq1(x)(y)
     */
    val program_5 = TopLevel(
                      Let( "x", Const( 10 ), 
                        Let( "y", Const( 15 ), 
                          Let( "sq1", FunDef( "x", FunDef( "y", Mult( Plus( Ident( "x" ), Ident( "y" ) ), Ident( "y" ) ) ) ),       
                            FunCall( FunCall( Ident( "sq1" ), Ident( "x" ) ), Ident( "y" ) ) 
                          ) 
                        ) 
                      ) 
                    );
    /*-- Again but with parts --*/
    val func_def_y = FunDef( "y", Mult( Plus( Ident( "x" ), Ident( "y" ) ), Ident( "y" ) ) );
    val func_def_x = FunDef( "x", func_def_y );
    val func_call_xy = FunCall( FunCall( Ident( "sq1" ), Ident( "x" ) ), Ident( "y" ) );
    val program_5_again = TopLevel( Let( "x", Const( 10 ), Let( "y", Const( 15 ), Let( "sq1", func_def_x, func_call_xy ) ) ) );
    assert( program_5 == program_5_again );

    /**
     * Function definition example 3
     *
     * let h = function(z)
     *        log(z)
     *    in
     * let g = function(y)
     *        y/2.0 + h(y*1.5)
     *    in
     * let f = function(x)
     *        1.0/x + g(x)
     *    in
     *        f(3.1415)
     */
    val program_6 = TopLevel(
                      Let( "h", FunDef( "z", Log( Ident( "z" ) ) ), 
                        Let( "g", FunDef( "y", Plus( Div( Ident( "y" ), Const( 2.0 ) ), FunCall( Ident( "h" ), Mult( Ident( "y" ), Const( 1.5 ) ) ) ) ), 
                          Let( "f", FunDef( "x", Plus( Div( Const( 1.0 ), Ident( "x" ) ), FunCall( Ident( "g" ), Ident( "x" ) ) ) ), 
                            FunCall( Ident( "f" ), Const( 3.1415 ) ) 
                          )
                        ) 
                      ) 
                    );
    /*-- Again but with parts --*/
    val func_def_z = FunDef( "z", Log( Ident( "z" ) ) );
    val func_def_y_ = FunDef( "y", Plus( Div( Ident( "y" ), Const( 2.0 ) ), FunCall( Ident( "h" ), Mult( Ident( "y" ), Const( 1.5 ) ) ) ) );
    val func_def_x_ = FunDef( "x", Plus( Div( Const( 1.0 ), Ident( "x" ) ), FunCall( Ident( "g" ), Ident( "x" ) ) ) );
    val func_call_f = FunCall( Ident( "f" ), Const( 3.1415 ) );
    val program_6_again = TopLevel( Let( "h", func_def_z, 
                                      Let( "g", func_def_y_, 
                                        Let( "f", func_def_x_, func_call_f ) 
                                      ) 
                                    )
                                  );
    assert( program_6 == program_6_again );
    /* --------------------------------------------------------------- */

    /**
     * Static vs. dynamic scoping for function calls
     * 
     * let x = 10 in 
     *  let f = function(y) y * x in
     *    let x = 15 in
     *      f(10)
     * 
     * val x = 10                     <-- x used in Static scoping
     * val f = ( y : Int ) => y * x
     * val x = 15                     <-- x used in Dynamic scoping
     * f( 10 )
     * 
     * Recall:
     * Static scoping : Resolve vars when a function is defined
     * Dynamic scoping : Resolve vars when a function is called
     * 
     * Error: Recursive function not in scope
     * 
     * let f 
     *       = function(x)    <---------------|
     *          if ( 0 >= x )                 |
     *              then 1                    |-exprA
     *          else                          |
     *              ( x - 1 ) * f( x - 1 )  <-|
     *      in
     *          f( 10 )         <-- exprB
     * 
     * Format: let f = exprA in exprB
     * 
     *    'f' is NOT in scope when calling f( x - 1 ) in
     *     exprA
     */
    val program_7_bad = TopLevel( 
                          Let( "f", FunDef( "x", 
                            IfThenElse( 
                              Geq( Const( 0 ), Ident( "x" ) ), Const( 1 ), 
                              Mult( Minus( Ident( "x" ), Const( 1 ) ), FunCall( Ident( "f" ), Minus( Ident( "x" ), Const( 1 ) ) ) ) 
                            ) ),
                            FunCall( Ident( "f" ), Const( 10 ) ) 
                          ) 
                        )
    /* --------------------------------------------------------------- */
    
    /**
     * Scala static scoping: Partial application of functions
     * 
     *  >> Can define functions whose args are bound at definition time
     *     
     *     Ex: define 'boundMatchFunc' which calls another function 
     *         'matchAgainstList' and passes it the statically scoped 
     *          reflist.
     * 
     *         This forces the refList passed at definition time to be
     *         used by 'boundMatchFunc' when it calls 'matchAgainstList'.
     * 
     *         The parameter of 'boundMatchFunc' takes any list of integers
     *         and passes it as lstB when calling 'matchAgainstList'
     * 
     *    So: Static scoping can 'lock in' parameters to arguments defined
     *    at definition time.
     */
    def matchAgainstList( ref_lst : List[ Int ] )( lstB: List[ Int ] ) = {
      def belongsToRefList( x : Int ) = {
        ref_lst.contains( x );
      }
      lstB.exists( belongsToRefList );
    }

    val refList = List( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );  // Passed bound arg at def time
    val boundMatchFunc : ( List[ Int ] => Boolean ) = matchAgainstList( refList ) ( _ );
    {
      val refList = List( 11, 12, 13, 14 );  // not used!
      boundMatchFunc( List( 5, 6, 7, 8 ) );  // Recursive call: pass lstB as ( _ )
    }
    /* --------------------------------------------------------------- */

    /**
     * Scala static scoping: callbacks
     * 
     *  >> Can define callbacks to handle events 
     *  >> Use partially applied function to bind extra info into the 
     *     callback function
     * 
     *     Ex: 
     *     KeyPressEventHandler takes no arguments and returns a function
     *     that takes an `Event` and performs some `Unit` action.
     * 
     *     In this case, 'keyPressedSoFar' is declared before the event
     *     callback is defined, so it is used to collect keypresses when 
     *     the callback is called.
     */
    def keyPressEventHandler() : ( Event => Unit ) ={

      var keysPressedSoFar : Set[ Int ] = Set(); // Mutable set of keys pressed
      
      // Match an 'Event' to an action 'Unit'
      def keyPressEventCallBack( event : Event ) : Unit = {
        event match{
          // Append keypress to the set of keysPressedSoFar
          case KeyPress(key) => keysPressedSoFar = keysPressedSoFar ++ Set( key )
          case PrintStatistics => println( "keys pressed so far: " + keysPressedSoFar );
          case _ => ();
        }
      }
      keyPressEventCallBack;
    } 
    /* --------------------------------------------------------------- */

    /**
     * Implementing dynamic scoping in Lettuce
     * 
     * >> Assume static scoping, and then 'force' dynamic scoping by 
     *    putting the dynamic scoped variable directly into the inline
     *    function definition wherever it is called.
     * 
     * Steps:
     *    (1) copy the function definition to be dynamically scoped
     *    (2) paste the function definition where the function is called
     *    (3) replace the dynamically scoped variable in the function
     *        definition with whatever the dynamically scoped var is
     * 
     * EX: Statically scoped
     *    let x = 10 in 
     *      let f = function(y) y * x in
     *        let x = 15 in
     *          f(10)
     * 
     * EX: Dynamically scoped:    
     *    let x = 10 in 
     *      let f = function(y) y * x in   <-- (1) copy f(y) returns y * x
     *        let x = 15 in
     *          (10 * x)      <-- (2) paste where 'f' is called
     *                            (3) replace 'x' with x = 15
     * 
     * EX: Statically scoped
     *    let y = 15 in
     *      let x = x * y in
     *        let f = function(z) z * y + x in
     *          let x = y in
     *            f(10) + f(15) 
     * 
     * EX: Dynamically scoped
     *    let y = 15 in
     *      let x = x * y in
     *        let f = function(z) z * y + x in  <-- (1) copy f(z) z * y + x
     *          let x = y in
     *            (10 * y + x) + (15 * y + x)  <-- (2) paste where 'f' is called 
     *                                             (3) replace 'z' with z=10, z=15
     * 
     * So -- we know to substitute the dynamic scoped variable 'x' in 
     * both examples: 'x = y' or 'x = 15' directly into the function 
     * definitions wherever a function call occurs ('inline'). 
     * 
     * But, if the user changes the dynamically scoped variable identifier from
     * 'x' to something else, e.g. 'xNew', unexpected behavior can occur. Why?
     * Because the 'inline' function definition that we copy/paste still 
     * references 'x', but we renamed it to 'xNew'
     */        
    // x_1 is statically typed in scala
    val x_1 = 10;
    val func_1 : ( Int => Int ) = _ * x_1;
    {
      val x_1 = 15;
      println( func_1( 10 ) ); // out: 100
    }
    // force x_1 to be dynamically typed
    val x_2 = 10;
    val func_2 : ( Int => Int ) = _ * x_1;
    {
      val x_2 = 15;
      println( ( 10 * x_2 ) ); // out: 150
    }
    /* --------------------------------------------------------------- */
  
    /**
     * Implementing function definitions in Lettuce
     * 
     *   >> What is a Closure? 
     *      > function(x) e with environment 𝜎
     *      > A function definition: 'function(x) { expr }' where x is the
     *        formal parameter and expr is the body of the function.
     *      > An environment: 𝜎 defining variables occuring in the body of
     *        the function 'expr'
     *      > function(x) e with environment 𝜎
     *      > Any var occuring freely requires an environment mapping
     * 
     * EX: function definition : function(y) x + y * y
     * 
     *     y  :   formal parameter
     *     x  :   identifier in 'expr' freely in the function definition
     * 
     *    Convert this function definition to a closure ...
     *    
     *    Closure( y, x + y * y, { x ↦ 10 } )
     *    
     *     y           :      formal parameter
     *     x + y * y   :      function definition
     *     { x ↦ 10 }  :     environment mapping
     * 
     * 
     * EX: function definition : function(x) function(y) x + y - 2 * z
     * 
     *      x                :       formal parameter
     *      function(y)      :       function definition
     *      x + y - 2 * z    :       function definition
     *      { z ↦ 100 }     :       environment mapping
     * 
     *    Closure( x, function(y) x + y - 2 * z, { z ↦ 100 } )
     * 
     *    Since z occurs freely it is mapped from the environment and so
     *    the closure requires a value for 'z' from the environment. 
     *    x and y are bound to the formal parameters of functions.
     * 
     * 
     */
    /* --------------------------------------------------------------- */

    /**
     * Implementing function calls in Lettuce
     * 
     * Recall: 'eval( e, 𝜎 ) = v' is evaluating 'e' under environment '𝜎'
     *          yields Value 'v'
     * 
     * 'Value' can be a Closure now, of the form: Closure( x, e, 𝜎 )
     * 
     * Rule for handling function definitions for static typing:
     * 
     *    ------------------------------------------------- (func def)
     *      eval(FuncDef(x, e), 𝜎) = Closure( x, e, 𝜎 )
     * 
     * Rule for handling function calls:
     * 
     *   eval(f-exp,𝜎)=Closure(p,e,𝜋),eval(arg-exp, 𝜎)=v2, v2!= error 
     *  -------------------------------------------------------------- (func call)
     *     eval(FuncCall(f-exp, arg-exp), 𝜎) = eval(e, 𝜋[p ↦ v2])
     * 
     * eval(f-exp,𝜎)=Closure(p,e,𝜋)  evaluate the function definition
     *  f-exp    :    the function we are calling with a Closure (definition)
     *      p    :    formal parameter
     *      e    :    body exprs
     *      𝜋    :    environment that was saved at define time
     * 
     * 
     * eval(arg-exp, 𝜎)=v2  evaluate the function argument
     *  arg-exp  :    the argument to the function call
     *      𝜎    :    environment mapping to evaluate arguments
     *      v2   :    Value that the evaluated argument
     * 
     * So, if we have a function definition with a closure, whose
     * arguments evaluate to some Value 'v2', then we can call the 
     * function. 
     * 
     * This is done by taking the function argument eval 'v2',
     * and including this value ( x -> v2 ) in the "closed" environment
     * of the function definition ( pi; in the closure ). With this
     * EXTENDED ENVIRONMENT in the closure from EVALUATING THE
     * ARGUMENT, expressions in the body of the closure can be
     * evaluated. 
     * 
     * Summary:
     * (1) eval the argument (x) in the function call, get value (v)
     * (2) Extend the environment of the function being called to include
     *     the evaluated argument (x -> v), i.e. (p -> v).
     * (3) Use the extended environment to evaluate the expressions
     *     in the body of the function, that is, 'e' in Closure( p, e, 𝜋 )
     * 
     * Error rules:
     * 
     *    function is not a closure
     * 
     *                       eval(f-exp, 𝜎) ∉ ℂ
     *  -------------------------------------------------------------- (no clos)
     *            eval(FuncCall(f-exp, arg-exp), 𝜎) = error
     * 
     * 
     *    function argument cannot be evaluated
     * 
     *      eval(f-exp,𝜎)=Closure(p,e,𝜋), eval(arg-exp, 𝜎) = error
     *  -------------------------------------------------------------- (no clos)
     *            eval(FuncCall(f-exp, arg-exp), 𝜎) = error
     */
    /* --------------------------------------------------------------- */
    val ret_4 : Value = evalProgram( program_4 );
    val ret_5 : Value = evalProgram( program_5 );
    val ret_6 : Value = evalProgram( program_6 );

    println( 
      s"""
      program_4 evaluated: ${ valConvert( ret_4 ) } 
      program_5 evaluated: ${ valConvert( ret_5 ) }
      program_6 evaluated: ${ valConvert( ret_6 ) }
      """ 
    );
  }
}
