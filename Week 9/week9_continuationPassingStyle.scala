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

sealed trait CPSResult[T]
case class Call[T](f: () => CPSResult[T]) extends CPSResult[T]
case class Done[T](v: T) extends CPSResult[T]
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

/**
 *  Continuation Passing Style
 *  Continue computation in function calls by passing other functions (continuation functions) as arguments.
 *
 *    (1) Every function has a "continuation" argument
 *
 *    (2) Continuation: a function that specifies what the caller wishes to do with the computed result
 *
 *    (3) The continuation function is passed the result of computation, and can do further computation
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
 */
object ContinuationPassing {
  // Example 1 -- Not continuation passing style
  def add(x: Int, y: Int, z: Int): Int = {
    x + y + z
  }

  def mult(x: Int, y: Int): Int = {
    x * y
  }

  def multAdd(x: Int, y: Int, z: Int): Int = {
    add(mult(x, y), y, z)
  }

  // Example 1 -- Continuation passing style
  def addK(x: Int, y: Int, z: Int, k: Int => Int): Int = {
    k(x + y + z) // (4) Pass to k=x=>x: x=v1=x*y, y=y_multAddK, z=z_multAddK
  }

  def multK(x: Int, y: Int, k: Int => Int): Int = {
    k(x * y) // (2) Multiply x*y, pass the result to k1
  }

  def multAddK(x: Int, y: Int, z: Int, k: Int => Int): Int = {
    def k1(v1: Int): Int = {
      addK(v1, y, z, k) // (3) Call addK, v1=x*y, k=x=>x
    }
    multK(x, y, k1) // (1) Call multK, pass continuation k1
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 2 -- Not continuation passing style
  def f1(x: Int): Int = {
    if (x <= 0)
      1
    else
      3 + f1(x - 10)
  }

  // Example 2 -- Continuation passing style
  def f1K(x: Int, k: Int => Int): Int = {
    if (x <= 0)
      k(1) // (2) Base case: Call k(1) and return the result
    else {
      def k1(v: Int): Int = {
        k(3 + v)
      }

      f1K(x - 10, k1) // (1) Recursion: f1k( x - 10, k( 3 + v ))
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 3 -- Not continuation passing style
  def func1(x: Int): Int = {
    val y = x * x
    val z = y + y - 5 * x
    if (z <= 0)
      1 // Return value
    else
      z // Return value
  }

  // Example 3 -- Continuation passing style
  def func1_k(x: Int, k: Int => Int): Int = {
    val y = x * x
    val z = y + y - 5 * x
    if (z <= 0)
      k(1) // Return values are passed to continuation function 'k'
    else
      k(z)
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 4 -- Not continuation passing style
  def func2(x: Int): Int = {
    if (x >= 0)
      func1(x + 1) // Tail call
    else {
      val y = x * x - 2
      func1(y) // Tail call
    }
  }

  // Example 4 -- Continuation passing style
  def func2_k(x: Int, k: Int => Int): Int = {
    if (x >= 0)
      func1_k(x + 1, k) // Tail calls are passed the continuation 'k'
    else {
      val y = x * x - 2
      func1_k(y, k)
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 5 -- Not continuation passing style
  def func3(x: Int, y: Int): Int = {
    if (x == 0)
      0
    else if (x > 0) {
      val s1 = 25
      val y1 = x * y + x - y
      s1 + y1 // Return value
    }
    else {
      val y1 = func2(x) // Function call that is not a tail call
      y1 + y - 2 * x
    }
  }

  // Example 5 -- Continuation passing style
  def func3_k(x: Int, y: Int, k: Int => Int): Int = {
    if (x == 0)
      k(0) // Return values are passed to continuation function 'k'
    else if (x > 0) {
      val s1 = 25
      val y1 = x * y + x - y
      k(s1 + y1) // Return values are passed to continuation function 'k'
    }
    else {
      def k1(y1: Int): Int = { // Wrap the return in continuation 'k1'
        k(y1 + y - 2 * x) // Return values are passed to continuation function 'k'
      }
      func2_k(x, k1) // Call the continuation version of 'func2' and pass continuation 'k1'
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  // Example 6 -- Not continuation passing style
  def func6(x: Int): Int = {
    val v1 = func3(x, x - 2) // (4) There exists a function call that is not a tail call
    /* (4) -- */
    val v2 = func3(x - 2, x) // (3) There exists a function call that is not a tail call
    /* (3) -- */
    val v3 = func2(v1) // (2) There exists a function call that is not a tail call
    /* (2) -- */
    val v4 = v1 + v2 + v3
    func3(v4, v3) // (1) Tail call
  }

  // Example 6 -- Continuation passing style abridged
  def func6_k2(x: Int, k: Int => Int): Int = {
    // **DONE**
    // val v1 = func3(x, x - 2)  // func3_k(x, x - 2, v1 => func3(x - 2, x, v2 => func2(v1, v => k(func3(v1 + v2 + v3, v3)))
    // val v2 = func3(x - 2, x)  // func3_k(x - 2, x, v2 => func2(v1, v => k(func3(v1 + v2 + v3, v3)))
    // val v3 = func2(v1)        // func2_k(v1, v3 => k(func3(v1 + v2 + v3, v3))
    // val v4 = v1 + v2 + v3     // k(func3(v1 + v2 + v3, v3))
    // func3(v4, v3)
    // **START**
    func3_k(x, x - 2,
      v1 =>
        func3_k(x - 2, x,
          v2 =>
            func2_k(v1,
              v3 =>
                k(func3(v1 + v2 + v3, v3))
            )
        )
    )
  }

  // Example 6 -- Continuation passing style
  def func6_k(x: Int, k: Int => Int): Int = {
    def k1(v1: Int): Int = { // (4) Wrap the return in continuation 'k1'
      def k2(v2: Int): Int = { // (3) Wrap the return in continuation 'k2'
        def k3(v3: Int): Int = { // (2) Wrap the return in continuation 'k3'
          val v4 = v1 + v2 + v3
          k(func3(v4, v3)) // (1) Tail calls are passed the continuation 'k'
        }
        func2_k(v1, k3) // (2) Call the continuation version of 'func2' and pass continuation 'k3'
      }
      func3_k(x - 2, x, k2) // (3) Call the continuation version of 'func3' and pass continuation 'k2'
    }
    func3_k(x, x - 2, k1) // (4) Call the continuation version of 'func3' and pass continuation 'k1'
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  /**
   * Polymorphic Continuations:
   *
   *    (1) Continuation passing style can change the return type of each continuation function
   *
   *    (2) These continuations need to return multiple types
   *
   *    (3) Define the continuation functions more generally using brackets for type parameters:
   *
   *                util_k[T1](x: Int, k: Int => T1): T1 = {}
   *
   *        This function takes an Int, and a function that takes an Int and returns type 'T1'
   *        Type parameter [T1] tells the compiler that util_k can take parameter of type T1 which can be anything.
   */
  // Example 7 -- Not continuation passing style
  def utilFunc(x: Int): Int = {
    x + 2 // return result of x+2 as an int
  }

  def call_1(x: String): String = {
    utilFunc(x.toInt).toString // return result of x+2 as a string
  }

  def call_2(x: Int): Float = {
    utilFunc(x).toFloat // return result of x+2 as a float
  }

  def mainFunc_1(x: Int): String = { // (1) Pass 'x' as an Int
    val v1 = call_1(x.toString) // (2) v1: Get x+2 as a String
    val v2 = call_2(x) // (3) v2: Get x+2 as a float
    v1 + v2.toString // (4) return: v1+v2 concatenated String
  }

  /**
   * Example 7 -- Polymorphic continuation
   * >>> All functions get 'k' as a formal parameter
   * >>> [T1], [T2], [T3] are generic types that each function will work with
   * >>> Return values are passed to continuation 'k'
   * >>> Tail calls are passed to continuation 'k'
   * >>> Non-tail functions 'f' pass their parameters and function 'k_' which executes lines below 'f'
   */
  def utilFunc_k[T1](x: Int, k: Int => T1): T1 = {
    k(x + 2) // Return values are passed to continuation function 'k'
  }

  def call_1_k[T2](x: String, k: String => T2): T2 = {
    utilFunc_k[T2](x.toInt, v => k(v.toString))
  }

  def call_2_k[T3](x: Int, k: Float => T3): T3 = {
    utilFunc_k[T3](x, v => k(v.toFloat))
  }

  def mainFunc_2(x: Int, k: String => String): String = {
    call_1_k[String]( // 'call_1' isn't tail call: pass x and everything below 'call_1' as 'v1'
      x.toString, v1 => {
        call_2_k[String]( // 'call_2' isn't tail call: pass x and everything below 'call_2' as 'v2'
          x, v2 => {
            k(v1 + v2.toString) // Return values are passed to continuation function 'k'
          })
      })
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  /**
   * Trampolines: A manual approach to tail call optimization
   *
   *    (1) Trampolines support continuation passing style in languages that do not do tail call optimization
   *
   * How?
   *
   *    (1) The tail call returns a closure of type 'CPSResult'
   *
   *    (2) While-loop tail calls while making sure the stack never grows
   *
   *    (3) CPS result can be:
   *
   *        >>> Call[T]: Call( f: () => CPSResult[T] ); call 'f' returns an object of type CPSResult[T]
   *
   *        >>> Done[T]: Encapsulates a value of type 'T'
   *
   * Example: Note -- Convert to CPS, then to trampoline; it seems to be easier that way
   *
   *      CPS form:
   *          def cFunc( x : .., k : .. => T ) : T = {   <----- k: Int => T, ret 'T'
   *              if ( base case )
   *                  return k( base args )              <----- Call 'k'
   *              else
   *                  ...
   *                  return tailCall( new_x, new_k )    <----- Call 'tailCall'
   *
   *      Trampolined:
   *          def tFunc( x : .., k : .. => CPSResult[T] ) : CPSResult[T] = {  <----- k: CPSResult[T], ret 'CPSResult[T]'
   *              if ( base case )
   *                  return Call( () => k( base args )                       <----- Return a call object
   *              else
   *                  return Call( () => tailCall( new_x, new_k_trampoline )  <----- Return a call object
   *
   *    (4) When the trampolined function is called, it returns a [ new function encapsulated inside a Call object ]
   *
   *    (5) Every function call 'f(..args..)' is replaced by 'Call( () => f(..args..) )'
   *
   *    (6) Call object encapsulates a closure: () => [whatever we were calling originally]. The unit closure delays
   *        computation so scala does not evaluate k( base args ) or tailCall(...) which defeats the purpose.
   */
  // OLD: CPS factorial function
  def factorial_k[ T ]( n: Int, k: Int => T ): T = {
    if ( n <= 0 )
      k( 1 ) // Will become: ( ) => { k( 1 ) }
    else {
      /**
       * Will become:
       * ( ) => t_factorial_k( n - 1, v => { Call( ( ) => { k( n * v ) } ) } )
       */
      factorial_k( n - 1, v => { k( n * v ) } )
    }
  }

  /**
   * NEW: Trampoline
   *
   * Return: Closure of type CPSResult[T] instead of continuation type 'T'
   *
   * sealed trait CPSResult[T]
   * case class Call[T](f: () => CPSResult[T]) extends CPSResult[T]
   * case class Done[T](v: T) extends CPSResult[T]
   */
  // (4) Create the trampoline: called once for each 'n' in n!, return nested functions each wrapped in a call object
  def t_factorial_k[ T ]( n: Int, k: Int => CPSResult[ T ] ): CPSResult[ T ] = {
    if ( n <= 0 )
      Call( ( ) => { k( 1 ) } )  // Wrap continuation call object
    else
      Call( ( ) => t_factorial_k( n - 1, v => { Call( ( ) => { k( n * v ) } ) } ) )  // Wrap recursive call object
  }
  // (1) Factorial function is called
  def factorial( n: Int ): Int = {
    def terminal_continuation( x: Int ): CPSResult[ Int ] = { Done( x ) }  // (2) Wrap 'x' in 'Done' call object
    var call_res = t_factorial_k( n, terminal_continuation )  // (3) Pass 'n' to 't_factorial_k', create the trampoline

    // (5) While loop: Run functions in the 'trampoline'
    var done = false
    while ( !done ) {
      call_res match {
        case Call( f ) => call_res = f( )  // goto the next nested 'Call' object, and call nested function 'f()'
        case Done( v ) => done = true      // Stop when terminal_continuation is reached
      }
    }
    // (6) Unwrap 'call_res = Done( v )' when the trampoline is over
    call_res match {
      case Done( v ) => v  // Return result of the factorial computation
      case _ => throw new MatchError( s"Catch all: Call() should not be found here")
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  /**
   * Example 9 -- Trampoline fibonacci
   */
  // OLD: CPS fibonacci function
  def fib_k[ T ]( n : Int, k: Int => T ): T = {
    if (n <= 2)
      k( 1 )  // Will become: Call( ( ) => k( 1 ) )
    else {
      /**
       * Will become:
       * Call( ( ) => tramp_fibonacci_k( n - 1,
       *    v1 =>
       *      Call( ( ) => tramp_fibonacci_k( n - 2,
       *          v2 =>
       *              Call( ( ) => k(v1 + v2) )
       *          )
       *      )
       */
      fib_k( n - 1,
        v1 =>
          fib_k( n - 2,
            v2 =>
              k( v1 + v2 )
          )
      )
    }
  }
  /**
   * NEW: Trampoline
   *
   * Return: Closure of type CPSResult[T] instead of continuation type 'T'
   *
   * sealed trait CPSResult[T]
   * case class Call[T](f: () => CPSResult[T]) extends CPSResult[T]
   * case class Done[T](v: T) extends CPSResult[T]
   */
  def tramp_fibonacci_k[T](n: Int, k: Int => CPSResult[T]): CPSResult[T] = {
    if (n <= 2) {
      // since fibonacci should not call k, it returns a Call object to trampoline, which will call it.
      Call( () => k( 1 ) )
    }
    else
    // was: fibonacci_k(n-1, v1 => fibonacci_k(n-2, v2 => k(v1+v2)))
    // make it into a call object
    // Do not forget to modify the continuation as well.
    // Wherever you see a function being called, mechanically replace it by Call( () => fun-being-called)
      Call( () => {
        tramp_fibonacci_k( n - 1,
          v1 =>
            Call(() => tramp_fibonacci_k( n - 2,
              v2 =>
                Call(() => k( v1 + v2 )
                ))
            ))
      })
  }

  def fibonacci(n: Int): Int = {
    /**
     * Call tramp_fibonacci_k, get a Call object closure
     * Terminal continuation t: Pattern matched by the while loop to stop computation
     */
    var res: CPSResult[Int] = tramp_fibonacci_k(n, t => Done(t) )

    /**
     * Pattern match the closure
     */
    var done = false
    while (!done) {
      res match {
        case Call(f) => res = f()    // Call the closure, compute, get next closure
        case Done(v) => done = true  // exit the trampoline
      }
    }

    /**
     * Return the result
     */
    res match {
      case Call(f) => throw new IllegalArgumentException("Fib Trampoline: This should never happen")
      case Done(v: Int) => v
    }
  }
}
/* -------------------------------------------------------------------------------------------------------------- */

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
    case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a closure")
  }

  /* -------------------------------------------------------------------------------------------------------------- */

  /*- Eval function for standard interpreter -*/
  def evalExpr(e: Expr, env: Map[String, Value]): Value = {
    def binaryOp(e1: Expr, e2: Expr)(fun: (Double, Double) => Double): NumValue = {
      val v1 = valueToNumber(evalExpr(e1, env))
      val v2 = valueToNumber(evalExpr(e2, env))
      val v3 = fun(v1, v2)
      NumValue(v3)
    }

    def applyArith1(e: Expr)(fun: Double => Double): NumValue = {
      val v = valueToNumber(evalExpr(e, env))
      val v1 = fun(v)
      NumValue(v1)
    }

    def boolOp(e1: Expr, e2: Expr)(fun: (Double, Double) => Boolean): BoolValue = {
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
      case Plus(e1, e2) => binaryOp(e1, e2)(_ + _)
      case Minus(e1, e2) => binaryOp(e1, e2)(_ - _)
      case Mult(e1, e2) => binaryOp(e1, e2)(_ * _)
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
            val new_env = closed_env + (x -> v2)
            evalExpr(closure_ex, new_env)
          }
          case _ => throw new IllegalArgumentException(s"Function call: $e1 does not evaluate to a closure")
        }
      }
    }
  }

  /* -------------------------------------------------------------------------------------------------------------- */

  def evalProgram(p: Program) = {
    val m: Map[String, Value] = Map[String, Value]()
    p match {
      case TopLevel(e) => evalExpr(e, m)
    }
  }

  /* -------------------------------------------------------------------------------------------------------------- */

  /*- Helpers for Continuation Passing Interpreter -*/
  def valueToNumberCPS[T](v: Value, k: Double => T): T = {
    v match {
      case NumValue(d) => k(d)
      case _ => throw new IllegalArgumentException(s"Error converting $v to number")
    }
  }

  def valueToBooleanCPS[T](v: Value, k: Boolean => T): T = {
    v match {
      case BoolValue(b) => k(b)
      case _ => throw new IllegalArgumentException(s"Error converting $v to boolean")
    }
  }

  def valueToClosureCPS[T](v: Value, k: Closure => T): T = {
    v match {
      case Closure(x, e, pi) => k(Closure(x, e, pi))
      case _ => throw new IllegalArgumentException(s"Error converting $v to closure")
    }
  }

  /*- Eval function for continuation passing interpreter -*/
  def evalExprCPS[T](e: Expr, env: Map[String, Value], k: Value => Value): Value = {

    /* Method to deal with binary arithmetic operations */
    def applyArith2(e1: Expr, e2: Expr)(fun: (Double, Double) => Double) = {
      /*
      val u1 = evalExpr(e1, env)
      val v1 = valueToNumber(u1)
      val u2 = evalExpr(e2, env)   ~~>  evalExprCPS[Value](e2, env, {u2 => valueToNumberCPS[Value]( ... )})}
      val v2 = valueToNumber(u2)   ~~>  valueToNumberCPS[Value](u2, {v2 => k(NumValue(fun(v1, v2))})
      val v3 = fun(v1, v2)
      NumValue(v3)                 ~~>  k(NumValue(fun(v1, v2))  Write as vals, convert to CPS from bottom to top
      */
      evalExprCPS[Value](e1, env, {
        u1 =>
          valueToNumberCPS[Value](u1, {
            v1 => {
              evalExprCPS[Value](e2, env, {
                u2 => {
                  valueToNumberCPS[Value](u2, {
                    v2 => {
                      k(NumValue(fun(v1, v2)))
                    }
                  })
                }
              })
            }
          })
      })
    }

    /* Helper method to deal with comparison operators */
    def applyComp(e1: Expr, e2: Expr)(fun: (Double, Double) => Boolean) = {
      /* val u1 = evalExpr(e1, env)
      val v1 = valueToNumber(u1)
      val u2 = evalExpr(e2, env)
      val v2 = valueToNumber(u2)
      val v3 = fun(v1, v2)
      BoolValue(v3)*/
      evalExprCPS[Value](e1, env, {
        u1 =>
          valueToNumberCPS[Value](u1, {
            v1 =>
              evalExprCPS(e2, env, {
                u2 =>
                  valueToNumberCPS[Value](u2, {
                    v2 => k(BoolValue(fun(v1, v2)))
                  })
              })
          })
      })
    }

    e match {
      case Const(f) => k(NumValue(f))
      case Ident(x) => {
        if (env contains x)
          k(env(x))
        else
          throw new IllegalArgumentException(s"Undefined identifier $x")
      }
      case Plus(e1, e2) => applyArith2(e1, e2)(_ + _)
      case Minus(e1, e2) => applyArith2(e1, e2)(_ - _)
      case Mult(e1, e2) => applyArith2(e1, e2)(_ * _)
      case Geq(e1, e2) => applyComp(e1, e2)(_ >= _)
      case Eq(e1, e2) => applyComp(e1, e2)(_ == _)

      /*- If/Then/Else -*/
      case IfThenElse(e1, e2, e3) => {
        evalExprCPS(e1, env, {
          case BoolValue(true) => evalExprCPS(e2, env, k)
          case BoolValue(false) => evalExprCPS(e3, env, k)
          case _ => throw new IllegalArgumentException(s"If-then-else condition expr: ${e1} is non-boolean")
        })
      }

      /*- Let Binding -*/
      case Let(x, e1, e2) => {
        evalExprCPS(e1, env, {
          v1 => {
            val env2 = env + (x -> v1) // create a new extended env
            evalExprCPS(e2, env2, k) // eval e2 under that.
          }
        })
      }

      /*- Function definitions and calls -*/
      case FunDef(x, e) => {
        k(Closure(x, e, env)) // Return a closure with the current enviroment.
      }
      case FunCall(e1, e2) => {
        evalExprCPS(e1, env, {
          v1 => {
            evalExprCPS(e2, env, {
              v2 => {
                v1 match {
                  case Closure(x, closure_ex, closed_env) => {
                    // First extend closed_env by binding x to v2
                    val new_env = closed_env + (x -> v2)
                    // Evaluate the body of the closure under the extended environment.
                    evalExprCPS(closure_ex, new_env, k)
                  }
                  case _ => throw new IllegalArgumentException(s"Function call error: expression $e1 does not evaluate to a closure")
                }
              }
            })
          }
        })
      }
    }
  }
  /* -------------------------------------------------------------------------------------------------------------- */

  def evalProgramCPS(p: Program) = {
    val m: Map[String, Value] = Map[String, Value]()
    p match {
      case TopLevel(e) => evalExprCPS(e, m, x => x)
    }
  }

  /* -------------------------------------------------------------------------------------------------------------- */

  def main( args : Array[ String ] ) : Unit = {

    /**
     * (1) 'multAddK' defines continuation 'k1' then calls 'multK':
     *
     *    multAddK( 1, 2, 3, x => 2 * x )  ~~>  multK( 1, 2, k1(v)=addK(v, 2, 3, k: x => 2 * x) )
     *
     * (2) 'multK' calls the continuation 'k1':
     *
     *    multK( 1, 2, k1(v)=addK(v, 2, 3, k: x => 2 * x) )
     *
     *      MultK is called which calls k( x * y ) ...
     *
     *    k1( 1 * 2 ) ~~> addK( 2, 2, 3, k = x => 2 * x )
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
    println(ContinuationPassing.func6_k2(3, x => x)) // Out: 3221322

    println(ContinuationPassing.mainFunc_1(5)) // Out: 77
    println(ContinuationPassing.mainFunc_2(5, x => x)) // Out: 77

    println(ContinuationPassing.mainFunc_1( 15 ))  // Out: 1717
    println(ContinuationPassing.mainFunc_2(15, x => x)) // Out: 1717
    /* ------------------------------------------------------------------------------------------------------------ */

    // Test 1 CPS interpreter
    val p1 = TopLevel(
      Let("square",                                 // let square =
        FunDef("x", Mult(Ident("x"), Ident("x"))),  //    function (x) x * x in
        FunCall(Ident("square"), Const(10))         //       square(10)
      )
    )
    println(evalProgramCPS(p1))

    // Test 2 CPS interpreter
    val x = Ident("x")
    val y = Ident("y")
    val fdef_inner = FunDef("y", Plus(x, Mult(y, y)))       //  let x = 10 in
    val fdef_outer = FunDef("x", fdef_inner)                //      let y = 15 in
    val call_expr = FunCall(FunCall(Ident("sq1"), x), y)    //        let sq1 = function(x) {
    val sq1_call = Let("sq1", fdef_outer, call_expr)        //                     function(y) { x + y * y }
    val lety = Let("y", Const(15), sq1_call)                //                  } in
    val letx = Let("x", Const(10), lety)                    //              sq1(x)(y)
    val p2 = TopLevel(letx)
    println(evalProgramCPS(p2))
    /* ------------------------------------------------------------------------------------------------------------ */

    // Test the trampoline for factorial and fibonacci
    println( s"\n4! is: ${ContinuationPassing.factorial( 4 )}" )
    println( s"\n fib(11) is: ${ContinuationPassing.fib_k( 11, x=>x )}" )
    /* ------------------------------------------------------------------------------------------------------------ */
  }
}