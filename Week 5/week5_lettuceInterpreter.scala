/** 
 * File: week5_lettuceInterpreter.scala
 * 
 * Working through the material on the Let language
 */

/**
 * Grammar for Lettuce types, expressions, and operations
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
/* --------------------------------------------------------------- */

/**
 * Grammar for Lettuce values that are evaluated
 *
 * Value -> NumValue( Double )
 *       |  BoolValue( Boolean )
 *       |  ErrorValue
 * 
 * Double -> -2 | -1 | 0 | 1 | 2 . . .
 * Boolean -> true | false
 * 
 */
sealed trait Value
case class NumValue( d : Double ) extends Value
case class BoolValue( b : Boolean ) extends Value
case object ErrorValue extends Value









/**
 * A very basic Lettuce eval function  -- no functions yet
 */
object lettuceEval {







  def main( args : Array[ String ] ) : Unit = {


    println( "Sanity check" );

  }

}