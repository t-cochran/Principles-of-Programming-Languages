/**
 * File: week8_callingConventionsMutable.scala
 *
 * General Notes:
 *
 *    (1) Function parameters are immutable in scala.
 *
 *    (2) Immutable instances passed as parameters by reference can mutable fields.
 *
 *    (3) var is a pointer to a memory cell that 'goes through' scopes, val is an evaluated value.
 *
 *    (4) Call by value: function argument is fully evaluated, then the value is passed to the function call.
 *
 *    (5) Pointers as parameters are immutable, but the values held at their address is mutable
 *
 *  ------------------------------------------------------------------------------------------------------------
 *
 *     Funcall( Ident, args )                      // Call function with Ident 'f'
 *           eval( Ident, σ ) => Ident: 'f'        // Evaluate the function identifier
 *           { f -> Closure( param, body, σ ) }    // Get the closure mapped to the function identifier
 *           π = σ + ( param -> args )             // Map the Closure parameter to the evaluated function args
 *           eval( body, π )                       // Evaluate the body of the function with arg mapping
 *
 *  ------------------------------------------------------------------------------------------------------------
 *
 *       Let bar = function(x)                     // Closure( param, body, σ ), π = σ + ( x -> ExplicitRef(0) )
 *          Let dummy1 = assignref(x, 25) in       // Eval( body, π ); x must be type ExplicitRef, not NumValue
 *             2 * deref(x)
 *       in
 *         Let z = newRef(12) in
 *           bar(z)                                // Eval arg z -> ExplicitRef(0) cell address j = 0, v = 12
 *
 * ------------------------------------------------------------------------------------------------------------
 *
 *    (6) Explicit references: Declare reference variables Value type ExplicitRef(j)
 *
 *                    NewRef(v: Expr) : Create a new explicit memory reference 'x'
 *                   DeRef(ref: Expr) : De-reference to get the value 'x'
 *      AssignRef(ref: Expr, v: Expr) : Assign a value to a explicit reference '( x, val )'
 *
 * ------------------------------------------------------------------------------------------------------------
 *
 *    (7) Implicit references : Declare a var as a reference Value type Reference(j)
 *
 *        LetVar(x: String, e1: Expr, e2: Expr) : Create a new 'var' binding implicit reference.
 *                AssignVar(x: String, v: Expr) : Assign a value to an implicit reference '( x, val )'
 *
 *        let var x = 10 in                        // create a new reference, bind to x, assign it 10
 *          let dummy = AssignVar( x, 20 ) in      // re-assign reference to 20
 *            x                                    // de-reference to return 20
 *
 * ------------------------------------------------------------------------------------------------------------
 *
 * Operational semantics: evaluating implicit references
 *
 *    (1) Value: ImmutableStore( nCells: Int, storeMap: Map[Int, Value] ) is an 'infinite' memory store
 *
 *    (2) Value: ExplicitRef(j) and Reference(j) are references to memory address 'j' in 'store'
 *
 *    (3) Immutable (val) bind Identifiers to an env, return new env:
 *
 *                                  eval( expr, env ) = ( value, new-env )
 *
 *    (4) Mutable (var) bind References(j) where 'j' is an address in 'store':
 *
 *                              eval( expr, env, store ) = ( value, new-store )
 *  ------------------------------------------------------------------------------------------------------------
 *
 * Constant rule:
 *
 *          ----------------------------------------------------- (constVar-mut)
 *               eval( Const( f ), 𝜎, store ) = ( f, store )
 *  ------------------------------------------------------------------------------------------------------------
 *
 * Identifier rule:
 *
 *      lookupCellValue : Given cell 'j' in memory store 's', return value in the cell 'v'
 *
 *            x ∈ 𝜎, 𝜎( x ) = Reference( j ), lookupCell( s, j ) = v
 *          ---------------------------------------------------------- (identVar-mut)
 *                     eval( Ident(x), 𝜎, s ) = ( v, s )
 *
 *       𝜎( x )  Reference( j ) : var x under env 𝜎 evaluates to a reference to cell 'j'
 *           lookupCell( s, j ) : Memory cell of address 'j' in store 's' has value 'v'
 *       eval( Ident(x), 𝜎, s ) : Evaluating identifier x under env 𝜎 and store s has value v
 *  ------------------------------------------------------------------------------------------------------------
 *
 * DeRef rule:
 *
 *      lookupCellValue : Given a memory reference 'Reference( j )', return its value 'v'
 *
 *            eval( e, 𝜎, s ) = ( r, s1 ), r = Reference( j ), lookupCell( s1, j ) = v
 *          ----------------------------------------------------------------------------- (DeRef)
 *                            eval( Deref( e ), 𝜎, s ) = ( v, s1 )
 *
 *                eval( e, 𝜎, s ) : Expr 'e' under env 𝜎 store 's' evaluates to reference 'r' of store 's1'
 *            lookupCell( s1, j ) : Evaluating identifier x under env 𝜎 and store s has value v
 *       eval( Deref( e ), 𝜎, s ) : De-referencing expression 'e' under env 𝜎, store s, yields value v
 *  ------------------------------------------------------------------------------------------------------------
 *
 * LetVar rule: Generate a new reference from 'let var x = e1 in e2'
 *
 *      createNewRef : Create new cell 'j' in store 's1' and assign it value 'v', return updated store 's2'
 *
 *         eval( e1, 𝜎, s ) = ( v, s1 ), v ≠ error, createNewRef( s1, v ) = ( j, s2 )
 *       ------------------------------------------------------------------------------- (letVar-mut)
 *        eval( LetVar( x, e1, e2 ), 𝜎, s ) = eval( e2, 𝜎[ x -> Reference( j ) ], s2 )
 *
 *                      eval( e1, 𝜎, s ) : Expr 'e1', under env 𝜎, store 's', evals to 'v' with new store 's1'
 *                 createNewRef( s1, v ) : create new cell in store 's1', with address 'j' and store 's2'
 *     eval( LetVar( x, e1, e2 ), 𝜎, s ) : eval e2 under: 𝜎[𝑥↦Reference(j)], store s2
 *  ------------------------------------------------------------------------------------------------------------
 *
 *  NewRef rule : Generate a new reference from 'NewRef(e)' containing value 'v' eval'd from 'e'
 *
 *      createNewRef : Create new cell 'j' in store 's1' and assign it value 'v', return updated store 's2'
 *
 *         eval( e, 𝜎, s ) = ( v, s1 ), v ≠ error, createNewRef( s1, v ) = ( j, s2 )
 *       ------------------------------------------------------------------------------- (NewRef)
 *                    eval( NewRef( e ), 𝜎, s ) = ( Reference( j ), s2 )
 *
 *               eval( e1, 𝜎, s ) : Expr 'e', under env 𝜎, store 's', evals to 'v' with new store 's1'
 *          createNewRef( s1, v ) : create new cell in store 's1', with address 'j' and store 's2'
 *      eval( NewRef( e ), 𝜎, s ) : eval NewRef results in a reference to the new cell 'j' in store 's2'
 *  ------------------------------------------------------------------------------------------------------------
 *
 * AssignVar rule:
 *
 *      assignToCell : Assign a new value 'v' to cell 'j', return updated memory store 's2'
 *
 *        x ∈ 𝜎, 𝜎(x) = Reference(j), eval(e, 𝜎, s) = (v, s1), assignToCell(s1, j, v) = s2
 *       ------------------------------------------------------------------------------------- (assignVar-mut)
 *                          eval(AssignVar(x, e), 𝜎, s) = (v, s2)
 *
 *           𝜎(x) = Reference(j) : var x under env 𝜎 maps to a reference to cell 'j'
 *                 eval(e, 𝜎, s) : Expr 'e' under env 𝜎 store 's' evaluates to value 'v' with new store 's1'
 *   assignToCell(s1, j, v) = s2 : assigning value 'v' to cell 'j' in store 's1' creates store 's2'
 *   eval(AssignVar(x, e), 𝜎, s) : under env 𝜎, store 's', var x = expr 'e' evals to 'v' in store 's2
 *  ------------------------------------------------------------------------------------------------------------
 *
 * AssignRef rule:
 *
 *      assignToCell : Assign a new value 'v' to cell 'j', return updated memory store 's3'
 *
 *   eval( e, 𝜎, s ) = ( r, s1 ), r = Reference(j), eval(e2, 𝜎, s1) = (v2, s2), assignToCell(s2, j, v2) = s3
 *    -------------------------------------------------------------------------------------------------- (AssignRef)
 *                       eval(AssignRef(e1, e2), 𝜎, s) = (v2, s3)
 *
 *              𝜎(x) = Reference(j) : var x under env 𝜎 maps to a reference to cell 'j'
 *                    eval(e, 𝜎, s) : Expr 'e' under env 𝜎 store 's' evaluates to reference 'r' of store 's1'
 *     assignToCell(s2, j, v2) = s2 : assigning value 'v2' from e2, to cell address 'j' from reference e1
 *    eval(AssignRef(e1, e2), 𝜎, s) : under env 𝜎, store 's', reference e1 is assigned value from expr 'e2'
 *  ------------------------------------------------------------------------------------------------------------
 */
trait Program
trait Expr
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

case class LetVar(x: String, e1: Expr, e2: Expr) extends Expr  // Create a new 'var' binding
case class AssignVar(x: String, v: Expr) extends Expr  // Assign a new value 'v' to var identifier 'x'

case class NewRef(v: Expr) extends Expr  // Create a new reference with value 'v'
case class DeRef(ref: Expr) extends Expr  // Return the value of reference 'ref'
case class AssignRef(ref: Expr, v: Expr) extends Expr // Assign reference ref the value e2
/* ------------------------------------------------------------------------------------------------------------- */

trait Value
case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(x: String, e: Expr, pi: Map[String, Value]) extends Value
case object ErrorValue extends Value

case class ImmutableStore( nCells: Int, storeMap: Map[Int, Value] )
case class ExplicitRef(j: Int) extends Value
case class Reference(j: Int) extends Value
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
};
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
          val v = env(x)  // Get Value mapped to the identifier
          v match {
            case ExplicitRef(j) => (v, store)  // Explicit reference created by NewRef
            case Reference(j) => // Var: De-reference by looking up address 'j' in memory store
              val v1 = MemoryOp.lookupCellValue(store, j)
              (v1, store)
            case _ => (v, store) // Val: Return the val mapped to the identifier
          }
        } else
          throw new IllegalArgumentException(s"Undefined identifier $x")

      /* Binary operations */
      case Plus(e1, e2) => binaryOp(e1, e2)(_ + _)
      case Minus(e1, e2) => binaryOp(e1, e2)(_ - _)
      case Mult(e1, e2) => binaryOp(e1, e2)(_ * _)
      case Geq(e1, e2) => boolOp(e1, e2)(_ >= _)
      case Eq(e1, e2) => boolOp(e1, e2)(_ == _)

      /* If/Then/Else */
      case IfThenElse(e1, e2, e3) =>
        val (v, store1) = eval(e1, env, store)
        v match {
          case BoolValue(true) => eval(e2, env, store1)
          case BoolValue(false) => eval(e3, env, store1)
          case _ => throw new IllegalArgumentException(
            s"If-then-else condition expr: ${e1} is non-boolean -- evaluates to ${v}"
          )
        }

      /* Let Binding */
      case Let(x, e1, e2) =>
        val (v1, store1) = eval(e1, env, store) // eval e1
        val env2 = env + (ArrowAssoc(x) -> v1) // create a new extended env
        eval(e2, env2, store1) // eval e2 under the extended env

      /* Function Definitions: Return the closure */
      case FunDef(x, e) => (Closure(x, e, env), store)

      /* Function Calls */
      case FunCall(e1, e2) =>
        val (v1, store1) = eval(e1, env, store)  // v1: Evaluate func identifier to Closure
        val (v2, store2) = eval(e2, env, store1) // v2: Evaluate func arguments
        v1 match {
          case Closure(param, body, closed_env) => {
            val pi = closed_env + (ArrowAssoc(param) -> v2)  // Extend env by mapping parameter to arguments
            eval(body, pi, store2)   // Evaluate the body of the closure under the extended environment
          }
          case _ => throw new IllegalArgumentException(
            s"Function call error: expression $e1 does not evaluate to a closure"
          )
        }

      /* NewRef: Create a new reference to the value from expr 'e' */
      case NewRef(e) => {
        val (v, store1) = eval(e, env, store)
        val (store2, j) = MemoryOp.createNewRef(store1, v)  // Create new reference of value 'v'
        (ExplicitRef(j), store2)
      }

      /* DeRef: Given expr 'e' = Reference(j), get value 'v' */
      case DeRef(e) => {
        val (v, store1) = eval(e, env, store)  // eval 'e' to get Reference(j)
        v match {
          case ExplicitRef(j) => {
            val v = MemoryOp.lookupCellValue(store1, j)  // Get value mapped to cell 'j' in store1
            (v, store1)
          }
          case _ => throw new IllegalArgumentException(
            s"Deref applied to an expr: $e val: $v that does not evaluate to a reference"
          )
        }
      }

      /* AssignRef: Given expr 'e2' = Reference(j), assign value 'e2' = v2 to store 'j' */
      case AssignRef(e1, e2) => {
        val (v1, store1) = eval(e1, env, store)  // eval 'e' to get Reference(j)
        v1 match {
          case ExplicitRef(j) => {
            val (v2, store2) = eval(e2, env, store1)  // eval 'e2' to get 'v2'
            val store3 = MemoryOp.assignToCell(store2, j, v2)  // map '
            (v2, store3)
          }
          case _ => throw new IllegalArgumentException(
            s"AssignRef applied to argument that is not a reference"
          )
        }
      }

      /* AssignVar: Assign a new value (Expr) 'e' to var identifier (String) 'x' */
      case AssignVar(x, e) =>
        val (v1, store1) = eval(e, env, store) // v1: Evaluate new var expression 'e'
        val v2 = if (env contains x) // v2: Current environment mapping of identifier 'x'
          env(x)
        else
          throw new IllegalArgumentException(s"Undefined identifier $x")
        v2 match {  // Match var to its Reference type
          case Reference(j) =>
            val store3 = MemoryOp.assignToCell(store1, j, v1) // assign expression value 'v1' to cell 'j'
            (v1, store3)
          case _ => throw new IllegalArgumentException(
            s"AssignVar applied to argument that is not a mutable var"
          )
        }

      /* LetVar binding: Create a new 'var' binding */
      case LetVar(x, e1, e2) => // let var x = e1 in e2
        val (v1, store1) = eval(e1, env, store) // v1: eval e1
        val (store2, j) = MemoryOp.createNewRef(store1, v1) // create a new cell containing v1
        val newEnv = env + (ArrowAssoc(x) -> Reference(j)) // map 'x' to cell 'j' and update the environment
        eval(e2, newEnv, store2) // evaluate e2 with the new environment and the new store containing cell 'j'
    }
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

object EvalProgram {
  def eval(p: Program) : Value = p match {
    case TopLevel(e) =>
      val (v1, s1) = EvalExpr.eval(
        e, // Program expression
        Map[String, Value](), // Empty environment
        ImmutableStore(nCells = 0, Map[Int, Value]()) // Empty memory store
      )
      v1
  }
}
/* ------------------------------------------------------------------------------------------------------------- */

object mutableNotes {
  def main( args : Array[ String ] ) : Unit = {
    /**
     *  let x = NewRef(10) in
     *      let y = DeRef(x) + 1 in
     *          let z = AssignRef(x, y) in
     *              z
     */
    val prog_0 = TopLevel(
      Let( "x", NewRef(Const(10.0)),
        Let("y", Plus(Const(1.0), DeRef(Ident("x"))),
          Let("z", AssignRef(Ident("x"), Ident("y")),
            Ident("z")
          )
        )
      )
    )
    println(s"Result1: ${EvalProgram.eval(prog_0)}")
    /**
     *  let var x = 10 in
     *      let y = AssignVar(x, 20) in
     *          x
     */
    val prog_1 = TopLevel(LetVar("x", Const(10), Let("y", AssignVar("x", Const(20)), Ident("x"))))
    println(s"Result = ${EvalProgram.eval(prog_1)}")
    /**
     * let var x = 10 in
     *    let g = function(y) { x } in
     *      let y = AssignVar(x, 20) iny
     *        g(y)
     */
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
    /**
     * let var f = function(x) { x + 10 } in
     *    let g = function(y) { y - 5 } in
     *      let d = f(10) in
     *        let z = AssignVar( f, g ) in
     *          d - f( 10 )
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
