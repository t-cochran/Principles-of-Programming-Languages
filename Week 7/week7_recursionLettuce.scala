/** 
 * File: week7_recursionLettuce.scala
 * 
 * Working through the material on the Let language with recursion
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
 *        |    Let( String, String, Expr, Expr )
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
case class LetRec( funcName : String, funcParam : String, 
                   defExpr : Expr, bodyExpr : Expr) extends Expr

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
 * Lettuce eval function that takes a Lettuce program written in
 * abstract syntax, and evaluates the program to return a result
 */
object lettuceRecur {

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
    /* --------------------------------------------------------------- */

    /**
     * Implementing recursion in Lettuce -- The problem and solution
     * 
     * let fact = function (x)
     *      if ( x <= 0 )
     *      then 1
     *      else x * fact( x - 1 )
     *    in
     *      fact( 20 )
     * 
     * Recall:  Let( s: String, defExpr: Expr, bodyExpr: Expr )
     * 
     * Let( "fact", FunDef( "x", IfThenElse( 
     *   Geq( Const( 0 ), Ident( "x" ) ), 
     *     Const( 1 ), 
     *   Mult( Ident( "x" ), 
     *      FunCall( Ident( "fact" ), Minus( Ident( "x" ), Const( 1 ) ) ) ) ) ), 
     *   FunCall( Ident( "fact" ), Const( 20 ) ) 
     * );
     * 
     * Recall eval: 
     *   case Let( x, e1, e2 ) => {
     *      val v1 = eval( e1, env )    // eval defining expr e1 under 𝜎
     *      val env_2 = env + (x -> v1) // extend env map 𝜎 to include x ↦ v1
     *      eval( e2, env_2 )           // eval body e2 with 𝜎[x ↦ v1] map
     * 
     * Problem: 
     * -----------
     * The identifier "x" or "fact" in a recursive function appears in 
     * the defining expression as well, where its not going to be part
     * of the environment. An attempt is made to find what 'x' or 'fact'
     * maps to during 'eval(e1, env)' but there is no 'x' or 'fact' in
     * env yet.
     * 
     * Explanation:
     * ---------------
     * In recursion, the Let( x, .. ) identifier 'x' that extends the
     * environment 𝜎[x ↦ v1] is NOT IN SCOPE when we evaluate the defining 
     * expr e1 if e1 contains a recursive call!! 
     * 
     * In other words, evaluating identifier 'x' under 𝜎 extends the 
     * environment with 𝜎[x ↦ v1]. This mapping is required when 
     * evaluating the body expression e2 (e.g. when calling a function 
     * the function being defined). 
     * 
     * The recursive function is in the defining expression e1, but we need 
     * to finish evaluating the defining expression e1 to get the new 
     * environment mapping 𝜎[x ↦ v1] in order to call the function in the
     * first place.
     * 
     * Solution:
     * ------------
     * Use a 'let rec' construct to indicate that recursion will occur:
     * 
     *  let rec <func_identifier> = function (<param_identifier>)
     *                                  <def_expr>
     *                              in
     *                                  <body_expr>
     * 
     * Grammars:
     *    Let( Identifier, Expr, Expr )
     *    LetRec( Identifier, Identifier, Expr, Expr )
     * 
     * Example:
     * -----------
     * let rec f = function(z)
     *                if ( 0 >= z )
     *                then 1
     *                else 1 + f(z - 1)
     *             in
     *                f(10)
     * 
     * LetRec( "f", "z", 
     *   IfThenElse( Geq( Const( 0 ), Ident( "z" ) ), 
     *               Const( 1 ),
     *               Plus( 
     *                 Const( 1 ), 
     *                 FunCall( 
     *                   Ident( "f" ), 
     *                   Minus( Ident( "z" ), Const( 1 ) ) 
     *                 ) 
     *               )
     *             ),
     *   FunCall( Ident( "f" ), Const( 10 ) )
     * );
     * 
     */
    val program_7 = LetRec( "f", "z", 
            IfThenElse( Geq( Const( 0 ), Ident( "z" ) ), 
                        Const( 1 ),
                        Plus( 
                          Const( 1 ), 
                          FunCall( 
                            Ident( "f" ), 
                            Minus( Ident( "z" ), Const( 1 ) ) 
                          ) 
                        )
                      ),
            FunCall( Ident( "f" ), Const( 10 ) )
    );

  }
}
