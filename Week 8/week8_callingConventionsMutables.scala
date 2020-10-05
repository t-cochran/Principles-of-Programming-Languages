/** 
 * File: week8_callingConventionsMutable.scala
 *
 * Working through the material on mutable and calling conventions
 */
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

case class LetVar(x: String, e1: Expr, e2: Expr) extends Expr
case class AssignVar(x: String, e: Expr) extends Expr
/* ------------------------------------------------------------------------------------------------------------- */

sealed trait Value
case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(x: String, e: Expr, pi: Map[String, Value]) extends Value
case object ErrorValue extends Value

case class ImmutableStore( nCells: Int, storeMap: Map[Int, Value] )
case class Reference(j: Int) extends Value
/* ------------------------------------------------------------------------------------------------------------- */

object TypeConvert {
  /**
   *  Given type Value return type Double
   */
  def valueToNumber( v: Value ): Double = v match {
    case NumValue(d) => d
    case _ => throw new IllegalArgumentException(s"Error: Convert v: $v to a number")
  }
  /**
   *  Given type Value return type Boolean
   */
  def valueToBoolean(v: Value): Boolean = v match {
    case BoolValue(b) => b
    case _ => throw new IllegalArgumentException(s"Error: Convert v: $v to a boolean")
  }
  /**
   *  Given type Value return type Closure
   */
  def valueToClosure(v: Value): Closure = v match {
    case Closure(x, e, pi) => Closure(x, e, pi)
    case _ => throw new IllegalArgumentException(s"Error: Convert v: $v to a closure")
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

object MemoryOp {
  /**
   * Create a new cell 'j' with value 'v':
   *
   * Given memory store 's' and value 'v', return new cell 'j' with value 'v' in updated store 's2'
   */
  def createNewRef(s: ImmutableStore, v: Value): (ImmutableStore, Int) = {
    val j = s.nCells // Assign 'j' a cell number
    val nMap = s.storeMap + (ArrowAssoc(j) -> v) // map cell (Int) 'j' to Value 'v' and update memory store
    val nStore = ImmutableStore(s.nCells + 1, nMap) // Make new store 's2' with one more cell 'j'
    (nStore, j)
  }
  /**
   * Fetch the value at cell 'j' :
   *
   * Given memory store 's' and cell 'j', return value 'v' in the cell
   */
  def lookupCellValue(s: ImmutableStore, j: Int): Value = {
    if (s.storeMap.contains(j)) {
      s.storeMap(j)  // return the Value mapped to cell 'j'
    } else {
      throw new IllegalArgumentException(s"Illegal lookup of nonexistent location $j")
    }
  }
  /**
   * Update the value at cell 'j' :
   *
   * Given store 's', cell 'j', value 'v', assign 'v' to cell 'j' and return the updated memory store 's2'
   */
  def assignToCell(s: ImmutableStore, j: Int, v: Value): ImmutableStore = {
    if (s.storeMap.contains(j)) {
      val nMap = s.storeMap + (ArrowAssoc(j) -> v) // Update the value mapped to cell 'j' and update memory store
      ImmutableStore(s.nCells, nMap)  // Make new store 's2' with updated cell 'j'
    } else {
      throw new IllegalArgumentException(s"Illegal assignment to nonexistent location $j")
    }
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

/**
 *  Evaluate expression 'e' in environment 'env' and memory store 'store'
 *
 *  Return: A tuple containing the evaluated 'Value' and memory store
 */
object EvalExpr {
  def eval(e: Expr, env: Map[String, Value], store: ImmutableStore): (Value, ImmutableStore) = {
    /**
     *  Helpers:
     *
     *  Evaluate expressions then pass the result to a function that performs an operation
     */
    def binaryOp(e1: Expr, e2: Expr)(f : (Double, Double) => Double) : (NumValue, ImmutableStore) = {
      val (v1, store1) = eval(e1, env, store)
      val (v2, store2) = eval(e2, env, store1)
      val v3 = f(TypeConvert.valueToNumber(v1), TypeConvert.valueToNumber(v2))
      (NumValue(v3), store2)
    }
    def boolOp(e1: Expr, e2: Expr)(f : (Double, Double) => Boolean) : (BoolValue, ImmutableStore) = {
      val (v1, store1) = eval(e1, env, store)
      val (v2, store2) = eval(e2, env, store1)
      val v3 = f(TypeConvert.valueToNumber(v1), TypeConvert.valueToNumber(v2))
      (BoolValue(v3), store2)
    }
    def unaryOp(e: Expr)(f : Double => Double) : (NumValue, ImmutableStore) = {
      val (v, store1) = eval(e, env, store)
      val v1 = f(TypeConvert.valueToNumber(v))
      (NumValue(v1), store1)
    }
    /* Pattern match expressions to helper methods */
    e match {
      case Const(f) => (NumValue(f), store)

      /* Identifiers: Mapped to either val or var */
      case Ident(x) =>
        if (env contains x) {
          val v = env(x)  // Get Value mapped to the identifier (e.g. x = ?? )
          v match {
            case Reference(j) => // Var: De-reference by looking up address 'j' in memory store
              val v1 = MemoryOp.lookupCellValue(store, j)
              (v1, store)
            case _ => (v, store) // Val
          }
        } else
          throw new IllegalArgumentException(s"Undefined identifier $x")

      case Plus(e1, e2) => binaryOp(e1, e2)(_ + _)
      case Minus(e1, e2) => binaryOp(e1, e2)(_ - _)
      case Mult(e1, e2) => binaryOp(e1, e2)(_ * _)
      case Geq(e1, e2) => boolOp(e1, e2)(_ >= _)
      case Eq(e1, e2) => boolOp(e1, e2)(_ == _)
      case IfThenElse(e1, e2, e3) =>
        val (v, store1) = eval(e1, env, store)
        v match {
          case BoolValue(true) => eval(e2, env, store1)
          case BoolValue(false) => eval(e3, env, store1)
          case _ => throw new IllegalArgumentException(s"If-then-else condition expr: ${e1} is non-boolean -- evaluates to ${v}")
        }
      case Let(x, e1, e2) =>
        val (v1, store1) = eval(e1, env, store) // eval e1
        val env2 = env + (ArrowAssoc(x) -> v1) // create a new extended env
        eval(e2, env2, store1) // eval e2 under that.
      case FunDef(x, e) =>
        (Closure(x, e, env), store) // Return a closure with the current environment.
      case FunCall(e1, e2) =>
        val (v1, store1) = eval(e1, env, store)
        val (v2, store2) = eval(e2, env, store1)
        v1 match {
          case Closure(x, closure_ex, closed_env) => {
            // First extend closed_env by binding x to v2
            val new_env = closed_env + (ArrowAssoc(x) -> v2)
            // Evaluate the body of the closure under the extended environment.
            eval(closure_ex, new_env, store2)
          }
          case _ => throw new IllegalArgumentException(s"Function call error: expression $e1 does not evaluate to a closure")
        }
      case AssignVar(x, e) => // x is a string -- name of identifier and e is Expr -- RHS of assignment
        val (v1, store1) = eval(e, env, store) // First evaluate e
        val v2 = if (env contains x) // Next, check x from the current environment
          env(x)
        else
          throw new IllegalArgumentException(s"Undefined identifier $x") // Trying to assign to an undeclared identifier
        v2 match {
          case Reference(j) => // x better be a reference in the current env.
            val store3 = MemoryOp.assignToCell(store1, j, v1) // assign to cell function in ImmutableStore API
            (v1, store3)
          case _ => throw new IllegalArgumentException(s"AssignVar applied to argument that is not a mutable var")
        }
      case LetVar(x, e1, e2) => // let var x = e1 in e2
        // This is the same treatment as let x = NewRef(e1) in e2 in ExplicitRef Language.
        val (v1, store1) = eval(e1, env, store) // evaluate e1
        val (store2, j) = MemoryOp.createNewRef(store1, v1) // create a new cell corresponding to the value of e1
        val newEnv = env + (ArrowAssoc(x) -> Reference(j)) // update the environment
        eval(e2, newEnv, store2) // evaluated e2 with the new environment and the new store.
    }
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

object EvalProgram {
  def eval(p: Program) : Value = p match {
    case TopLevel(e) => {
      // Start with empty environment and empty store
      val (v1, s1) = EvalExpr.eval(e, Map(), new ImmutableStore(nCells = 0, Map()))
      v1
    }
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

object Notes {
  /**
   * Function parameters are immutable in scala.
   *
   * In 'Wrapper', 'x' is a mutable field. The mutable field of the immutable instance can be changed
   * in a function. In this case, you are changing mutable values held by the reference and not the
   * reference itself which is immutable.
   *
   * Note: In scala, 'var' is a pointer/reference to a memory cell, while 'val' is simply a value
   */
  case class Wrapper( var x : Int )  // x is a mutable field
  def bar_2( z : Wrapper ) : Int = {
    z.x = 25   // re-assign the mutable field 'x'
    2 * z.x
  }

  /**
   * Call by value:
   *    func(arg) the arg is fully evaluated, then the value is passed to the function call.
   *  ------------------------------------------------------------------------------------------------------------
   *    Let x = 10 in
   *      Let f = function(z) { 2 * z } in
   *          f( x + 20 )
   *
   *    Recall:
   *      Funcall( Ident, args )                         // Call function with Ident: 'f', args: 30
   *            eval( Ident, σ ) => Ident: 'f'           // Evaluate the function identifier, get Closure
   *            { f -> Closure( param, body, σ ) }
   *            π = σ + ( param -> args )                // Map Closure parameter to function arg: 30
   *            eval( body, π )                          // Evaluate the body of the function with arg mapping
   *
   *    (1) Evaluate 'arg' x + 20  => env: { x -> 10 }, so arg = 30, map parameter to arg: {z -> 30}
   *    (2) Call the function 'f' on the value of arg = 30 function(z = 30) { 2 * z } evals to 60
   *  ------------------------------------------------------------------------------------------------------------
   *       NewRef : Create a new explicit memory reference 'x'
   *       DeRef  : De-reference to get the value 'x'
   *    AssignRef : Assign a value to a reference '( x, val )'
   *
   *    Let bar = function(x)                     // Closure( param, body, σ ), π = σ + ( x -> NewRer(12) )
   *       Let dummy1 = assignref(x, 25) in       // Eval( body, π )
   *          2 * deref(x)
   *    in
   *      Let z = newRef(12) in
   *        bar(z)                                // Funcall: Ident 'f', arg 'z -> NewRer(12)'
   * 
   *    (1) Evaluate 'arg' z => env: { z -> newRef(12) } a reference to memory cell '0' with value '12'
   *    (2) Call function 'bar' by mapping Closure parameter to function arg: 'param -> ref(0)', eval 'bar' body
   *
   *    When 'bar' evaluates the function body via 'eval( body, π )', we pass 'ref(0)' to function 'dummy1'.
   *    'dummy1' assigns 'ref(0)' value 25, then de-references it and computes 2 * 25 = 50
   *
   *    Conclusion:
   *      Pointers passed to functions can have their values mutated. The reference (memory address) is immutable,
   *      but the value held at the address is mutable. Hence, class fields can change and referenced values can
   *      be re-assigned.
   * ------------------------------------------------------------------------------------------------------------
   *  EX: Why does this program fail?
   *
   *        def bar( x : Int ) : Int = {
   *            x = 25    // Parameter x is an immutable val
   *            2 * x
   *        }
   *        var z = 12    // z is not a reference; it evaluates to value 12
   *        bar( z )
   *
   *  The lettuce example would fail for similar reasons:
   *
   *    Let bar = function(x)                     // Closure( param, body, σ ), π = σ + ( x -> 12 )
   *      Let dummy1 = assignVar(x, 25) in        // Eval( body, π )
   *          2 * x
   *    in
   *      Let z = 12 in
   *        bar(z)                                // Funcall: Ident 'f', arg 'z -> 12'
   *
   *    (1) Evaluate 'arg' z => env: { z -> 12 } the value '12
   *    (2) Call function 'bar' by mapping Closure parameter to function arg: 'param -> 12', eval 'bar' body
   *
   *    When 'bar' evaluates the function body via 'eval( body, π )', we pass '12' to function 'dummy1'.
   *    'dummy1' assigns '12' value '25'. This does not work because in π { param -> 12 } maps to a value.
   *  ------------------------------------------------------------------------------------------------------------
   *  Call by value with objects:
   *      Double / Int : call by value is the value itself
   *            object : call by value is a reference to object
   *
   *  Note: If accessing a struct in C/C++, passing by value requires copying the struct; better to pass by ref
   */

  /* ------------------------------------------------------------------------------------------------------------- */

  /**
   * Mimic mutable vars in scala (e.g. implicit references, var x = 1; x = x + 1 )
   *
   *  Vars : References to cells in memory (pointers). Var changes "go through" scopes, so change the var at
   *         function call time, this is reflected in the call even if static scoping occurred.
   *
   *  Implicit references : Don't need to use NewRef to create a reference; simply declare a var as a reference
   *
   *    'Let var' : A binding to specify that the bound variable will be an implicit reference.
   *  'AssignVar' : Assign a value to a reference '( x, val )'
   *
   *                                    let var x = <expr> in
   *                                        <body expr>
   *  Examples:
   *
   *    let var x = 10 in                        // create a new reference, bind to x, assign it 10
   *      let dummy = AssignVar( x, 20 ) in      // re-assign reference to 20
   *        x                                    // de-reference to return 20
   *
   *    let var x = 10 in                        // create a new reference, bind to x, assign it 10
   *        let g = function(y)                  // body is a function(y) returns x
   *              x
   *          in
   *            let dummy = AssignVar( x, 20 ) in   // dummy assigns x to 20
   *               g( dummy )                       // pass x->20 to g; call g; g returns x; therefore x = 20
   *
   *  Note: 'let var' can be re-assigned via AssignVar( var, _ ); 'let _' cannot since it is immutable
   */
  /* ------------------------------------------------------------------------------------------------------------- */

  /**
   * Abstract syntax for mutable references
   *
   * Expr -> LetVar( Ident, Expr, Expr )
   *       | AssignVar( Ident, Expr )
   *  ------------------------------------------------------------------------------------------------------------
   *
   * Operational semantics: How to evaluate implicit references?
   *    (1) Make a new value type for references, like we did for function definitions (Closures)
   *          -> Now 'Expr' will eval() to: NumVal, BoolVal, Closure, Reference
   *          -> Reference( j ) references a cell number 'j' in the store
   *    (2) Define an abstract notion of memory called: 'store'
   *
   *    eval( expr, env ) = ( value, new-env )
   *       - Immutable values ( let ... ) are bound to values in the environment
   *
   *    eval( expr, env, store ) = ( value, new-store )
   *        - Mutable values (let var ... ) are about to 'Reference( j )' where 'j' is an address in 'store'
   *  ------------------------------------------------------------------------------------------------------------
   *
   * Stores: memory addresses of natural numbers (0, 1, 2...)
   *
   * Constant rule:
   *
   *          ----------------------------------------------------- (constVar-mut)
   *               eval( Const( f ), 𝜎, store ) = ( f, store )
   *
   * Identifier rule:
   *
   *      lookupCellValue : Given cell 'j' in memory store 's', return value in the cell 'v'
   *
   *            x ∈ 𝜎, 𝜎( x ) = Reference( j ), lookupCell( s, j ) = v
   *          ---------------------------------------------------------- (identVar-mut)
   *                     eval( Ident(x), 𝜎, s ) = ( v, s )
   *
   *      𝜎( x )  Reference( j ) : var x under env 𝜎 evaluates to a reference to cell 'j'
   *           lookupCell( s, j ) : Memory cell 'j' in store 's' has value 'v'
   *       eval( Ident(x), 𝜎, s ) : Evaluating identifier x under env 𝜎 and store s has value v
   *
   * LetVar rule: Generate a new reference from 'let var x = e1 in e2'
   *
   *      createNewRef : Create new cell 'j' in store 's1' and assign it value 'v', return updated store 's2'
   *
   *         eval( e1, 𝜎, s ) = ( v, s1 ), v ≠ error, createNewRef( s1, v ) = ( j, s2 )
   *       ------------------------------------------------------------------------------- (letVar-mut)
   *        eval( LetVar( x, e1, e2 ), 𝜎, s ) = eval( e2, 𝜎[ x -> Reference( j ) ], s2 )
   *
   *                      eval( e1, 𝜎, s ) : Expr 'e1' under env 𝜎, store 's' evals to 'v' with new store 's1'
   *                 createNewRef( s1, v ) : create new cell in 's1', reference cell 'j' and store 's2'
   *     eval( LetVar( x, e1, e2 ), 𝜎, s ) : eval e2 under: 𝜎[𝑥↦Reference(j)], store s2
   *
   * AssignVar rule:
   *
   *      assignToCell : Assign a new value 'v' to cell 'j', return updated memory store 's2'
   *
   *        x ∈ 𝜎, 𝜎(x) = Reference(j), eval(e, 𝜎, s) = (v, s1), assignToCell(s1, j, v) = s2
   *       ------------------------------------------------------------------------------------- (assignVar-mut)
   *                          eval(AssignVar(x, e), 𝜎, s) = (v, s2)
   *
   *    𝜎(x) = Reference(j) : var x under env 𝜎 maps to a reference to cell 'j'
   *          eval(e, 𝜎, s) : Expr 'e' under env 𝜎 store 's' evaluates to value 'v' with new store 's1'
   *   assignToCell(s1, j, v) = s2 : assigning value 'v' to cell 'j' in store 's1' creates store 's2'
   *   eval(AssignVar(x, e), 𝜎, s) : under env 𝜎, store 's', var x = expr 'e' evals to 'v' in store 's2
   *  ------------------------------------------------------------------------------------------------------------
   *
   */
  def main( args : Array[ String ] ) : Unit = {
    /* let var x = 10 in
    let dummy = AssignVar(x, 20) in
        x
        */
    val x1 = Ident("x")
    val e1 = Let("dummy", AssignVar("x", Const(20)), x1)
    val e2 = LetVar("x", Const(10), e1)
    val prog_1 = TopLevel(e2)
    println(s"Result = ${EvalProgram.eval(prog_1)}")

    /*~~~
let var x = 10 in
   let g = function (y)
            x
        in
       let dummy = AssignVar(x, 20) in
            g (dummy)
~~~*/
    val g = Ident("g")
    val dummy = Ident("dummy")
    val x2 = Ident("x")
    val e3 = FunCall(g, dummy)
    val e4 = Let("dummy", AssignVar("x", Const(20)), e3)
    val gdef = FunDef("y", x2)
    val e5 = Let("g", gdef, e4)
    val e6 = LetVar("x",  Const(10), e5)
    val prog_2 = TopLevel(e6)
    println(s"Result = ${EvalProgram.eval(prog_2)}")

    /*---
let var f = function (x) x + 10 in
  let g = function (y) y - 5 in
    let d = f(10) in
       let dummy = AssignVar(f, g) in
          d - f(10)
          */
    val d1 = Ident("d")
    val f1 = Ident("f")
    val g1 = Ident("g")
    val x3 = Ident("x")
    val y3 = Ident("y")
    val e11 = Minus(d1, FunCall(f1, Const(10)))
    val e12 = Let("dummy", AssignVar("f", g1), e11)
    val e13 = Let("d", FunCall(f1, Const(10)), e12)
    val gdefg = FunDef("y", Minus(y3, Const(5)))
    val e14 = Let("g", gdefg, e13)
    val fdef = FunDef("x", Plus(x3, Const(10)))
    val e15 = LetVar("f", fdef, e14)
    val prog_3 = TopLevel(e15)
    println(s"Result = ${EvalProgram.eval(prog_3)}")
  }
}
