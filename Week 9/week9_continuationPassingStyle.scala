/**
 *  week9_continuationPassingStyle.scala
 */
import scala.annotation.tailrec

sealed trait Program
sealed trait Expr
case class TopLevel(e: Expr) extends Program
case class Const(v: Double) extends Expr
case class Ident(s: String) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Geq(e1: Expr, e2:Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class IfThenElse(e: Expr, eIf: Expr, eElse: Expr) extends Expr
case class Let(s: String, defExpr: Expr, bodyExpr: Expr) extends Expr

case class FunDef(param: String, bodyExpr: Expr) extends Expr
case class FunCall(funCalled: Expr, argExpr: Expr) extends Expr
/* -------------------------------------------------------------------------------------------------------------- */

sealed trait Value
case class NumValue(d: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(x: String, e: Expr, pi: Map[String, Value]) extends Value
case object ErrorValue extends Value
/* -------------------------------------------------------------------------------------------------------------- */

object TailRecReview {
  def foo( x : Int ) : Int = {
    x - 15
  }

  @tailrec
  def rec_1( x : Int ) : Int = {
    if ( x <= 0 )
      x
    else
      rec_1( x - 10 )
  }

  @tailrec
  def rec_2( y : Int = 0, x : Int ) : Int = {
    if ( x <= 10 )
      y
    else
      rec_2( y + 1, x - 10 )
  }

  // Not tail recursive
  def rec_3( x : Int ) : Int = {
    if ( x <= 10 )
      x - 10
    else
      1 + rec_3( x - 10 )
  }

  // Not tail recursive
  def rec_4( x : Int ) : Int = {
    if ( x <= 10 )
      x - 5
    else
      rec_4( rec_4( x - 10 ) )
  }

  @tailrec
  def rec_5( x : Int ) : Int = {
    if ( x <= 0 )
      foo( x )
    else
      rec_5( foo( x ) )
  }

  // Not tail recursive
  def rec_6( x : Int ) : Int = {
    if ( x <= 0 )
      foo( x )
    else
      foo( rec_6( x - 5 ) )
  }

  // Not tail recursive
  def factorial( n : Int ) : Int = {
    if ( n <= 0 )
      1
    else
      n * factorial( n - 1 )
  }

  @tailrec
  def factorialAcc( acc : Int = 1, n : Int ) : Int = {
    if ( n <= 0 )
      acc
    else
      factorialAcc( acc * n, n - 1 )
  }

  // Not tail recurisve
  def fibonacci( n : Int ) : Int = {
    if ( n < 2 )
      1
    else
      fibonacci( n - 1 ) + fibonacci( n - 2 )
  }

  @tailrec
  def fibonacciAcc( n : Int, acc1 : Int = 1, acc2 : Int = 1 ) : Int = {
    if (n <= 0)
      acc1
    else if (n == 1)
      acc2
    else
      fibonacciAcc( n - 1, acc2, acc1 + acc2 )
  }
}
/* -------------------------------------------------------------------------------------------------------------- */
object ContinuationPassing {

  // Example 1 -- Not continuation passing style
  def add( x : Int, y : Int, z : Int ) : Int = {
    x + y + z
  }
  def mult( x : Int, y : Int ) : Int = {
    x * y
  }
  def multAdd( x : Int, y : Int, z : Int ) : Int = {
    add( mult( x, y ), y, z )
  }

  // Example 1 -- Continuation passing style
  def addK( x : Int, y : Int, z : Int, k : Int => Int ) : Int = {
    k( x + y + z ) // (4) Pass to k=x=>x: x=v1=x*y, y=y_multAddK, z=z_multAddK
  }
  def multK( x : Int, y : Int, k : Int => Int ) : Int = {
    k( x * y )  // (2) Multiply x*y, pass the result to k1
  }
  def multAddK( x : Int, y : Int, z : Int, k : Int => Int ) : Int = {
    def k1( v1 : Int ) : Int = {
      addK( v1, y, z, k )  // (3) Call addK, v1=x*y, k=x=>x
    }
    multK( x, y, k1 )  // (1) Call multK, pass continuation k1
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 2 -- Not continuation passing style
  def f1( x : Int ) : Int = {
    if ( x <= 0 )
      1
    else
      3 + f1( x - 10 )
  }

  // Example 2 -- Continuation passing style
  def f1K( x : Int, k : Int => Int ) : Int = {
    if ( x <= 0 )
      k( 1 )  // (2) Base case: Call k(1) and return the result
    else {
      def k1( v : Int ) : Int = {
        k( 3 + v )
      }
      f1K( x - 10, k1 )  // (1) Recursion: f1k( x - 10, k( 3 + v ))
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 3 -- Not continuation passing style
  def func1( x : Int ) : Int = {
    val y = x * x
    val z = y + y - 5 * x
    if ( z <= 0 )
      1  // Return value
    else
      z  // Return value
  }

  // Example 3 -- Continuation passing style
  def func1_k( x : Int, k : Int => Int ) : Int = {
    val y = x * x
    val z = y + y - 5 * x
    if ( z <= 0 )
      k( 1 )  // Return values are passed to continuation function 'k'
    else
      k( z )
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 4 -- Not continuation passing style
  def func2( x : Int ) : Int = {
    if ( x >= 0 )
      func1( x + 1 )  // Tail call
    else {
      val y = x * x - 2
      func1( y )  // Tail call
    }
  }

  // Example 4 -- Continuation passing style
  def func2_k( x : Int, k : Int => Int ) : Int = {
    if ( x >= 0 )
      func1_k( x + 1, k )  // Tail calls are passed the continuation 'k'
    else {
      val y = x * x - 2
      func1_k( y, k )
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 5 -- Not continuation passing style
  def func3( x : Int, y : Int ) : Int = {
    if ( x == 0 )
      0
    else if ( x > 0 ) {
      val s1 = 25
      val y1 = x * y + x - y
      s1 + y1  // Return value
    }
    else {
      val y1 = func2(x)  // Function call that is not a tail call
      y1 + y - 2 * x
    }
  }

  // Example 5 -- Continuation passing style
  def func3_k( x : Int, y : Int, k : Int => Int ) : Int = {
    if ( x == 0 )
      k( 0 )  // Return values are passed to continuation function 'k'
    else if ( x > 0 ) {
      val s1 = 25
      val y1 = x * y + x - y
      k( s1 + y1 )  // Return values are passed to continuation function 'k'
    }
    else {
      def k1( y1 : Int ) : Int = {  // Wrap the return in continuation 'k1'
        k( y1 + y - 2 * x )  // Return values are passed to continuation function 'k'
      }
      func2_k( x, k1 )  // Call the continuation version of 'func2' and pass continuation 'k1'
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 6 -- Not continuation passing style
  def func6( x : Int ) : Int = {
    val v1 = func3( x, x - 2 )  // (4) There exists a function call that is not a tail call
    /* (4) -- */
    val v2 = func3( x - 2, x )  // (3) There exists a function call that is not a tail call
    /* (3) -- */
    val v3 = func2( v1 )  // (2) There exists a function call that is not a tail call
    /* (2) -- */
    val v4 = v1 + v2 + v3
    func3( v4, v3 )  // (1) Tail call
  }

  // Example 6 -- Continuation passing style
  def func6_k( x : Int, k : Int => Int ) : Int = {
    def k1( v1 : Int ) : Int = {  // (4) Wrap the return in continuation 'k1'

      def k2( v2 : Int ) : Int = {  // (3) Wrap the return in continuation 'k2'

        def k3( v3 : Int ) : Int = {  // (2) Wrap the return in continuation 'k3'

          val v4 = v1 + v2 + v3
          k( func3( v4, v3 ) )   // (1) Tail calls are passed the continuation 'k'

        }

        func2_k( v1, k3 )  // (2) Call the continuation version of 'func2' and pass continuation 'k3'
      }

      func3_k( x - 2, x, k2 )  // (3) Call the continuation version of 'func3' and pass continuation 'k2'

    }

    func3_k( x, x - 2, k1 )  // (4) Call the continuation version of 'func3' and pass continuation 'k1'
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 7 -- Not continuation passing style
  def utilFunc( x : Int ) : Int = {
    x + 2  // Return value
  }
  def call_1( x : String ) : String = {
    utilFunc( x.toInt )
            .toString
  }
  def call_2( x : Int ) : Float = {
    utilFunc( x )
            .toFloat
  }
  def mainFunc_1( x : Int ) : String = {
    val v1 = call_1( x.toString )
    val v2 = call_2( x )
    v1 + v2.toString
  }

  // Example 7 -- Polymorphic continuation
  def utilFunc_k[T1]( x : Int, k : Int => T1 ) : T1 = {
    k( x + 2 )  // Return values are passed to continuation function 'k'
  }
  def call_1_k[T2]( x : String, k : String => T2 ) : T2 = {
    utilFunc_k[T2]( x.toInt, v => k( v.toString ) )
  }
  def call_2_k[T3]( x : Int, k : Float => T3 ) : T3 = {
    utilFunc_k[T3]( x, v => k( v.toFloat ) )
  }
  def mainFunc_2( x : Int, k : String => String ) : String = {
    call_1_k[ String ]( x.toString, v1 => {
      call_2_k[ String ]( x, v2 => {
          k( v1 + v2.toString )
      })
    })
  }
}

/**
 *  Continuation Passing Style
 *
 *    (1) Every function has a "continuation" argument
 *
 *    (2) Continuation: a function that specifies what the caller wishes to do with the computed result
 *
 *    (3) The continuation function is passed the result of computation, and can do further computation
 *
 * EX:
 *
 *    def func( x : Int ) : Int = {      --|
 *        ...                              |____ Standard function
 *        return result                    |
 *    }                                  --|
 *
 *    def func( x : Int, k : Int => Int ) : Int = {    --|
 *        ...                                            |____ Continuation passing style function
 *        k( result )                                    |
 *    }                                                --|
 *
 *  (4) Argument ( k : Int => Int ) is a continuation; it takes the result and specifies what to do with it.
 *
 * Properties of continuation passing style:
 *
 *    (1) Every function involved has an extra argument 'k' for the continuation function 'k : Int => Int'
 *
 *    (2) The first call is passed a "termination continuation", typically 'x => x'
 *
 *    (3) Each code path ends with a function call, and all calls are tail calls
 *
 * Polymorphic Continuations:
 *
 *    (1) Continuation passing style can change the return type of each continuation function
 *
 *    (2) These continuations need to return multiple types
 *
 *    (3) Define the continuation functions more generally using brackets:
 *
 *                util_k[T1](x: Int, k: Int => T1): T1 = {}
 *
 *        This function takes an Int, and a function that takes an Int and returns type 'T1'
 */
object Notes {
  /*- Helpers for standard interpreter -*/
  def valueToNumber(v: Value): Double = v match {
    case NumValue(d) => d
    case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a number")
  }
  def valueToBoolean(v: Value): Boolean = v match {
    case BoolValue(b) => b
    case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a boolean")
  }
  def valueToClosure(v: Value): Closure = v match {
    case Closure(x, e, pi) => Closure(x, e, pi)
    case _ =>  throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a closure")
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  /*- Eval function for standard interpreter -*/
  def evalExpr(e: Expr, env: Map[String, Value]): Value =  {
    def binaryOp(e1: Expr, e2: Expr) (fun: (Double , Double) => Double) : NumValue = {
      val v1 = valueToNumber(evalExpr(e1, env))
      val v2 = valueToNumber(evalExpr(e2, env))
      val v3 = fun(v1, v2)
      NumValue(v3)
    }
    def applyArith1(e: Expr) (fun: Double => Double) : NumValue = {
      val v = valueToNumber(evalExpr(e, env))
      val v1 = fun(v)
      NumValue(v1)
    }
    def boolOp(e1: Expr, e2: Expr) (fun: (Double, Double) => Boolean) : BoolValue = {
      val v1 = valueToNumber(evalExpr(e1, env))
      val v2 = valueToNumber(evalExpr(e2, env))
      val v3 = fun(v1, v2)
      BoolValue(v3)
    }
    e match {
      /*- Constants and identifiers -*/
      case Const(f) => NumValue(f)
      case Ident(x) => {
        if (env contains x)
          env(x)
        else
          throw new IllegalArgumentException(s"Undefined identifier $x")
      }

      /*- Arithmetic -*/
      case Plus(e1, e2) => binaryOp(e1, e2) ( _ + _ )
      case Minus(e1, e2) => binaryOp(e1, e2) ( _ - _ )
      case Mult(e1, e2) =>  binaryOp(e1, e2) (_ * _)

      /*- Comparison -*/
      case Geq(e1, e2) => boolOp(e1, e2)(_ >= _)
      case Eq(e1, e2) => boolOp(e1, e2)(_ == _)

      /*- If/Then/Else -*/
      case IfThenElse(e1, e2, e3) => {
        val v = evalExpr(e1, env)
        v match {
          case BoolValue(true) => evalExpr(e2, env)
          case BoolValue(false) => evalExpr(e3, env)
          case _ => throw new IllegalArgumentException(s"If-then-else: ${e1} is non-boolean -- evaluates to ${v}")
        }
      }

      /*- Let Binding -*/
      case Let(x, e1, e2) => {
        val v1 = evalExpr(e1, env)
        val env2 = env + (x -> v1)
        evalExpr(e2, env2)
      }

      /*- Function definitions and calls -*/
      case FunDef(x, e) => {
        Closure(x, e, env)
      }
      case FunCall(e1, e2) => {
        val v1 = evalExpr(e1, env)
        val v2 = evalExpr(e2, env)
        v1 match {
          case Closure(x, closure_ex, closed_env) => {
            val new_env = closed_env + ( x -> v2)
            evalExpr(closure_ex, new_env)
          }
          case _ => throw new IllegalArgumentException(s"Function call: $e1 does not evaluate to a closure")
        }
      }
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  def evalProgram(p: Program) = {
    val m: Map[String, Value] = Map[String,Value]()
    p match {
      case TopLevel(e) => evalExpr(e, m)
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  /*- Helpers for Continuation Passing Interpreter -*/
  def valueToNumberCPS[T](v: Value, k: Double => T) : T = {
    v match {
      case NumValue(d) => k(d)
      case _ => throw new IllegalArgumentException(s"Error converting $v to number")
    }
  }
  def valueToBooleanCPS[T](v: Value, k: Boolean => T) : T = {
    v match {
      case BoolValue(b) => k(b)
      case _ => throw new IllegalArgumentException(s"Error converting $v to boolean")
    }
  }
  def valueToClosureCPS[T](v: Value, k: Closure => T) : T = {
    v match {
      case Closure(x, e, pi) => k(Closure(x, e, pi))
      case _ => throw new IllegalArgumentException(s"Error converting $v to closure")
    }
  }



  def main( args : Array[ String ] ) : Unit = {

    /**
     * (1) 'multAddK' defines continuation 'k1' then calls 'multK':
     *
     * multAddK( 1, 2, 3, x => 2 * x )  ~~>  multK( 1, 2, k1(v)=addK(v, 2, 3, k1) )
     *
     * (2) 'multK' calls the continuation 'k1':
     *
     * multK( 1, 2, k1(v)=addK(v, 2, 3, k1) )  ~~>  k1( 1 * 2 ) ~~> addK( 2, 2, 3, k = x => 2 * x )
     *
     * (3) addK calls the continuation 'k':
     *
     * addK( 2, 2, 3, k = x => 2 * x )  ~~>  k( (2 + 2 + 3) => 2 * ( 2 + 2 + 3 ) )
     * k( 7 => 2 * 7 ) ~~> 14
     */
    println(ContinuationPassing.multAddK(1, 2, 3, x => 2 * x)) // Out: 14

    /**
     * (1) 'f1K' gets the argument and identity function 'k'
     *
     * (2) Recursive case: define continuation 'k1( v ) => k( 3 + v )'
     *
     * (3) Recursive case: Call 'f1k( x - 10, k1 )'
     *
     * Key: Each recursive call re-defines 'k' to 'k1'; so, k1 changes for each recursive call:
     *
     * k( 3 + v ) ~~> k( 3 + k( 3 + v ) ) ~~> k( 3 + k( 3 + k( 3 + v ) ) ) ~~> etc...
     *
     * (4) Base case: Call k(1), which substitutes v = 1 into the above continuation; For x = 40, we have:
     *
     * k( 3 + k( 3 + k( 3 + k( 3 + v=1 ) ) ) )
     * 4 + 3 + 3 + 3 = 13
     */
    println(ContinuationPassing.f1(40)) // Out: 13
    println(ContinuationPassing.f1K(40, x => x)) // Out: 13
    /* ------------------------------------------------------------------------------------------------------------ */

    println(ContinuationPassing.func6(3)) // Out: 3221322
    println(ContinuationPassing.func6_k(3, x => x)) // Out: 3221322

    println(ContinuationPassing.mainFunc_1(5)) // Out: 77
    println(ContinuationPassing.mainFunc_2(5, x => x)) // Out: 77

  }
}