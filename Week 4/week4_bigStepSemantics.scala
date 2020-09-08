/** 
 * File: week3_bigStepSemantics.scala 
 * 
 * Working through the material on big step semantics expressions
 */


/**
  * Grammar for arithmetic expressions: 
  * 
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
  * 
  * Double -> -2 | -1 | 0 | 1 | 2 . . .
  * Identifier -> [a-zA-Z][a-zA-Z0-9_]
  */
sealed trait Expr;

/* Inductive definition for arithmetic expressions */
case class Const(f: Double) extends Expr;
case class Ident(s: String) extends Expr;
case class Plus( e1: Expr, e2: Expr ) extends Expr;
case class Minus(e1: Expr, e2: Expr) extends Expr;
case class Mult(e1: Expr, e2: Expr ) extends Expr;
case class Div(e1: Expr, e2: Expr) extends Expr;
case class Log(e: Expr) extends Expr;
case class Exp(e: Expr) extends Expr;
case class Sine(e: Expr) extends Expr;
case class Cosine(e: Expr) extends Expr;
/* --------------------------------------------------------------- */

/** 
 * Define values of evaluated expressions 
 *
 * Value -> Number( Double )
 *       |  Error
 * 
 * Double -> -2 | -1 | 0 | 1 | 2 . . .
 */
sealed trait Value;
case object Error extends Value;
case class Number( c : Double ) extends Value;
/* --------------------------------------------------------------- */


object EvalExprObject {

  /**
   * Eval function
   * 
   * Take an environment that maps variable identity strings to values.
   * Use the environment mapping to evaluate arithmetic expressions.
   */
  def evalExpr( expr : Expr, env : Map[ String, Double ] ) : Value = {

    ( expr ) match {

      case Const( num ) => return Number( num );  // Simply return constants

      case Ident( str ) => {                      // Return the number mapped in env
        if ( env.contains( str ) ) {
          return Number( env( str ) );
        }
        else {
          return Error;
        }
      }

      /* Evaluate expressions containing two sub-expressions */
      case Plus( exp1, exp2 ) => {  
        val v1 = this.evalExpr( exp1, env );
        val v2 = this.evalExpr( exp2, env );
        ( v1, v2 ) match {
          case ( _, Error ) => return Error;
          case ( Error, _ ) => return Error;
          case ( Number( x ), Number( y ) ) => {
            return Number( x + y );
          }
        }
      }

      case Minus( exp1, exp2 ) => {
        val v1 = this.evalExpr( exp1, env );
        val v2 = this.evalExpr( exp2, env );
        ( v1, v2 ) match {
          case ( _, Error ) => return Error;
          case ( Error, _ ) => return Error;
          case ( Number( x ), Number( y ) ) => {
            return Number( x - y );
          }
        }
      }
      case Mult( exp1, exp2 ) => {
        val v1 = this.evalExpr( exp1, env );
        val v2 = this.evalExpr( exp2, env );
        ( v1, v2 ) match {
          case ( _, Error ) => return Error;
          case ( Error, _ ) => return Error;
          case ( Number( x ), Number( y ) ) => {
            return Number( x * y );
          }
        }
      }
      case Div( exp1, exp2 ) => {
        val v1 = this.evalExpr( exp1, env );
        val v2 = this.evalExpr( exp2, env );
        ( v1, v2 ) match {
          case ( _, Error ) => return Error;
          case ( Error, _ ) => return Error;
          case ( Number( x ), Number( y ) ) => {
            if ( y == 0 ) return Error;  
            else return Number( x / y );
          }
        }
      }

      /* Evaluate expressions containing one sub-expression */
      case Log( exp ) => { 
        val v = this.evalExpr( exp, env );
        ( v ) match {
          case Error => return Error;
          case Number( c ) if c > 0.0 => return Number( math.log( c ) );
          case _ => return Error;
        }
      }
      case Exp( exp ) => {
        val v = this.evalExpr( exp, env );
        ( v ) match {
          case Error => return Error;
          case Number( c ) => return Number( math.exp( c ) );
          case _ => return Error;
        }
      }
      case Sine( exp ) => {
        val v = this.evalExpr( exp, env );
        ( v ) match {
          case Error => return Error;
          case Number( c ) => return Number( math.sin( c ) );
          case _ => return Error;
        }
      }
      case Cosine( exp ) => {
        val v = this.evalExpr( exp, env );
        ( v ) match {
          case Error => return Error;
          case Number( c ) => return Number( math.cos( c ) );
          case _ => return Error;
        }
      }
    }
  }
  /* --------------------------------------------------------------- */

  def main( args : Array[ String ] ) : Unit = {

    /* Check evalExpr on expression: x + y */
    val y1 : Expr = Ident("y1");
    val x1 : Expr = Ident("x1");

    val exp_1 : Expr = Plus( x1, y1 );
    val env : Map[ String, Double ] = Map( "y1" -> 2.0, "x1" -> 10.0 );

    println( evalExpr( exp_1, env ) );

    /* Check evalExpr on expression: cos(x) + sin(y) + exp(x - (y + z)) */
    val y : Expr = Ident( "y" );        
    val x : Expr = Ident( "x" );
    val z : Expr = Ident( "z" );
    val exp_2 : Expr = Plus( Cosine( x ), Plus( Sine( y ), Exp( Minus( x, Plus( y, z ) ) ) ) );
    val env_2 : Map [ String, Double ] = Map( "y" -> 1.5, "x" -> 2.0, "z" -> 2.8 );
    println( evalExpr( exp_2, env_2 ) );

    /**
     * Check evalExpr on expression Errors:
     */
    println( evalExpr( Log( Const( 0 ) ) , env_2 ) );     // Error: Log(0)
    println( evalExpr( Div( x , Const( 0 ) ), env_2 ) );  // Error: Div by 0

  }
}