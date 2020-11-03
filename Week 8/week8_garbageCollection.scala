/**
 *  Week 8 - Covering the material on garbage collection
 */
object garbageCollectionNotes {
  /**
   * Garbage Collection:
   *    -> Need a delete operation so we do not run out of memory cells for explicit and implicit references
   *    -> Create an implicit reference (let var), it uses a memory store but goes out of scope and isn't used again:
   *
   *    let x = ( let var y = 25 in     // new cell created for y
   *                let z = y + 25 in
   *                    y + z
   *            ) in                    // y goes out of scope; cell still exists
   *        let var w = 25 in
   *            w + x
   *
   *    -> Reclaim memory cells in the store that are no longer required (not reachable) by the program
   *
   * Unreachable cells:
   *    let var x = 10 in                                           // store 0: x -> Ref(0): 10
   *        let var y = ( let var z = 15 in                         // store 1: z -> Ref(1): 15
   *                        let dummy = assignvar(z, x - 5) in      // store 1: z -> Ref(1): 5, dummy -> 5
   *                            z                                   // z -> Ref(1): 5
   *                    ) in                                        // z out of scope
   *                y                                               // store 2: y -> Ref(2): 5
   *
   *    -> store 0, store 2 still reachable via 'x' and 'y'
   *    -> store 1 is not reachable by 'z' when it goes out of scope
   *    -> Function definitions (Closures) that reference a memory store whenever they are called
   *
   * Types of garbage:
   *    -> Syntactic Garbage: Unreachable cell(s) because identifiers out of scope
   *    -> Semantic Garbage: Unreachable cell(s) because identifiers out of scope or not referenced again
   *
   * Reference counting:
   *    -> **OLD** The memory store simply maps addresses (j: Int) to values (v: Value)
   *          case class ImmutableStore( nCells: Int, storeMap: Map[Int, Value])
   *
   *    -> **NEW** The memory store as an array of tuples
   *          type Address = Int
   *          type RefCount = Int
   *          type Store = Array[(Value, RefCount)]
   *
   *    -> RefCount: A count of how many references to an address exist in the environment
   *    -> Re-Use references with a RefCount of 0 since they are no longer used (semantic garbage)
   *
   * Implementing reference counting:
   *    -> If a new identifier is in scope and bound to a reference, increment its reference count
   *    -> If a let binding has finished evaluating, decrement its reference count
   *    -> If a reference count for a cell is zero, re-use the cell on the next cell allocation
   *
   *    let x = Newref(10) in                                             // addr 0: x -> Ref(0): 10 RefCnt: 1
   *        let y = Newref( let z = Newref(15) in                         // addr 1: z -> Ref(1): 15 RefCnt: 1
   *                          let dummy = Assignref(z, deref(x) - 5) in   // addr 1: z -> Ref(1): 5  RefCnt: 1
   *                              Deref(z) + 20                           // ...z about to go out of scope
   *                       ) in                                           // addr 1: z -> Ref(1): 5  RefCnt: 0
   *            Deref(y)                                                  // addr 1: y -> Ref(1): 25 RefCnt: 1
   */
  /*--------------------------------------------------------------------------------------------------------------*/

  /**
   * Layered Environment to handle recursion
   * e.g., EmptyEnv -> Extend("x", 3, EmptyEnv) -> ExtendRec("fact", "n", bodyExpr, Extend("x", 3, EmptyEnv))
   */
  sealed trait Environment
  case object EmptyEnvironment extends Environment
  case class Extend(x: String, v: Value, env: Environment) extends Environment
  case class ExtendRec(f: String, x: String, e: Expr, env: Environment) extends Environment

  def lookupEnv(x: String, env: Environment):Value = env match {
    case EmptyEnvironment => throw new IllegalArgumentException(s"could not find identifier $x")
    case Extend(x1, v1, _) if (x == x1) => v1
    case Extend(_, _, env1) => lookupEnv(x, env1)
    case ExtendRec(f, param, e, _) if (x == f) => Closure(param, e, env)
    case ExtendRec(_, _, _, env1 ) => lookupEnv(x, env1)
  }
  /*--------------------------------------------------------------------------------------------------------------*/

  /* Values */
  sealed trait Value
  case object ErrorValue extends Value
  case class NumValue(d: Double) extends Value
  case class BoolValue(b: Boolean) extends Value
  case class Reference(j: Address) extends Value
  case class Closure(id: String, e: Expr, env: Environment) extends Value

  /* Expressions */
  sealed trait Expr
  case class Const(f: Double) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Ident(x: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Mult(e1: Expr, e2: Expr) extends Expr
  case class Geq(e1: Expr, e2: Expr) extends Expr
  case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
  /*--------------------------------------------------------------------------------------------------------------*/

  /* Let Binding */
  case class Let(x: String, e1: Expr, e2: Expr ) extends Expr

  /* Functions */
  case class Fundef(x: String, body: Expr) extends Expr
  case class Funcall(e1: Expr, e2: Expr) extends Expr
  case class LetRec(f: String, x: String, e1: Expr, e2: Expr) extends Expr

  /* Implicit References */
  case class LetVar(x: String, e1: Expr, e2: Expr) extends Expr
  case class AssignVar(x: String, e1: Expr) extends Expr

  /* Explicit References */
  case class Newref(e: Expr) extends Expr
  case class Deref(e: Expr) extends Expr
  case class Assignref(e1: Expr, e2: Expr) extends Expr
  /*--------------------------------------------------------------------------------------------------------------*/

  /* Memory store */
  type Address = Int
  type RefCount = Int
  type Store = Array[(Value, RefCount)]               // Memory store: Collection of (Value, RefCount) tuples
  def emptyStore: Store = Array[(Value, RefCount)]()  // Create a new empty memory store
  /*--------------------------------------------------------------------------------------------------------------*/

  /**
   *  Add a new value (val: Value) into the memory store (Store: Array[(Value, RefCount)])
   */
  def createNewCell(s: Store, v: Value): (Store, Address) = {
    val j = s.indexWhere(x => (x._2 == 0)) // Find first cell with 0 reference count
    if (j >= 0) {                          // if such a cell found
      s(j) = (v, 0)                        // allocate the value v to that cell, ref count is still 0
      (s, j)                               // return the new store and the address
    }
    else {                                // If no cell with a 0 reference count exists
      val newAddr = s.length              // we will create a new cell at the very end
      val s1 = s :+ (v, 0)                // create a new store with a new cell at the end
      (s1, newAddr)                       // return the new store and the address at which we inserted our new elt.
    }
  }

  /**
   *  Fetch a value from the memory store (Store: Array[(Value, RefCount)]) at address 'j'
   */
  def lookup(s: Store, j: Address): Value = s(j)._1

  /**
   *  Increment the RefCount at address 'j' in store 's'
   */
  def incrementReference(s: Store, j: Address): Store = {
    val sValue = s(j)                // Assert the current RefCount is geq zero
    assert(sValue._2 >= 0)
    s(j) = (sValue._1, sValue._2+1)  // Index store 's' with address 'j', and increment Refcount (Value, RefCount+1)
    sValue._1 match {                // Check the Value at s(j)
      case Reference(addr)  =>  traverseChainAndIncrement(s, addr, Set(j))  // Reference: Increment its RefCount too
      case _ => s                                                           // Not Reference: Return the store
    }
  }

  /**
   *  Decrement the RefCount at address 'j' in store 's'
   */
  def decrementReference(s: Store, j: Address): Store = {
    val sValue = s(j)                // Assert the current RefCount is geq one
    assert(sValue._2 >= 1)
    s(j) = (sValue._1, sValue._2-1)  // Index store 's' with address 'j', and increment Refcount (Value, RefCount-1)
    sValue._1 match {                // Check the Value at s(j)
      case Reference(addr) =>  traverseChainAndDecrement(s, addr, Set(j))  // Reference: Decrement its RefCount too
      case _ => s                                                          // Not Reference: Return the store
    }
  }

  /**
   *  Assign value (v: Value) to memory store (Store: Array[(Value, RefCount)]) at address 'j'
   */
  def assignCell(s: Store, j: Address, v: Value): Store = {
    /*- Check the old value at address j -*/
    val sValue = s(j)          // Old (Value, RefCount) at address j
    val oldValue = sValue._1   // Old Value
    val s1 = {
      oldValue match {
        case Reference(addr) => decrementReference(s, addr)  // Reference: decrement refcount in store
        case _ => s                                          // Not a reference, store is not changed
      }
    }

    /*- Replace the content of the store with value v -*/
    s1(j) = (v, sValue._2)
    val s2 = v match {
      case Reference(addr) => incrementReference(s1, addr)  // Reference: Increment refcount in store
      case _ => s1                                          // Not a reference, store is not changed
    }
    s2                                                      // Return the new store with value v at address j
  }

  /**
   *  Traverse nested (i.e. chained) references if the value referenced at a store is another reference
   *  Need to traverse the chain of references when decrementing or incrementing the refcount at the root
   */
  def traverseChainAndIncrement(s: Store, addr: Address, visited: Set[Address]) =
    traverseChainAndOp(s, addr, visited, true)

  def traverseChainAndDecrement(s: Store, addr: Address, visited: Set[Address]) =
    traverseChainAndOp(s, addr, visited, false)

  def traverseChainAndOp(s: Store, addr: Address,
                         visited: Set[Address], increment: Boolean): Store = {
    if (visited.contains(addr)){
      println("Cyclic pattern of references detected in the store.")
      s
    } else {
      val sValue = s(addr)
      s(addr) = {
        if (increment) {
          (sValue._1, sValue._2 + 1)
        } else {
          assert(sValue._2 >= 1)
          (sValue._1, sValue._2 - 1)
        }
      }
      sValue._1 match {
        case Reference(addr2) => {
          val newVisited = visited + addr
          traverseChainAndIncrement(s, addr2, newVisited)
        }
        case _ => s
      }
    }
  }
  /*--------------------------------------------------------------------------------------------------------------*/
  /**
   * Helper: Extract the number from a NumValue in an evaluated expression
   */
  def toNumber(v : (Value, Store)): Double = {
    v match {
      case (NumValue( num: Double ), _ ) => num
      case _ => throw new IllegalArgumentException(s"Error: toNumber argument $v must be of type NumValue")
    }
  }

  /* Eval function */
  def evalExpr(e: Expr, env: Environment, s: Store): (Value, Store) = {

    def binopNumHelper(e1: Expr, e2: Expr) ( foo: (Double, Double) => Value ) = {
      val (v1, s1) = evalExpr(e1, env, s)
      v1 match {
        case NumValue(f1) => {
          val (v2, s2) = evalExpr(e2, env, s1)
          v2 match {
            case NumValue(f2) => (foo(f1, f2), s2)
            case _ => throw new IllegalArgumentException("Cannot op non numeric values")
          }
        }
        case _ => throw new IllegalArgumentException("Cannot op non numeric values")
      }
    }

    e match {
      case Const(f) => (NumValue(f), s)
      case Bool(b) => (BoolValue(b), s)
      case Ident(x) => { (lookupEnv(x, env), s) }
      case Plus(e1, e2) => binopNumHelper(e1, e2) ((f1, f2) => NumValue(f1 + f2))
      case Mult(e1, e2) => binopNumHelper(e1, e2) ((f1, f2) => NumValue(f1 * f2))
      case Geq(e1, e2) => binopNumHelper(e1, e2) ((f1, f2) => BoolValue(f1 >= f2))
      case IfThenElse(e1, e2, e3) => {
        val (v1, s1) = evalExpr(e1, env, s)
        v1 match {
          case BoolValue(true) => evalExpr(e2, env, s1)
          case BoolValue(false) => evalExpr(e3, env, s1)
          case _ => throw new IllegalArgumentException("If then else condition needs to be a boolean value")
        }
      }

      case Let(x, e1, e2) => {
        /*- Evaluate e1 -*/
        val (v1, s1) = evalExpr(e1, env, s)                        // eval e1 to v1 under the current environment
        val s2: Store =
          v1 match {
            case Reference(addr) => incrementReference(s1, addr)   // v1 is a Reference: Increment reference count
            case _ => s1                                           // v1 is not a Reference: return the store
          }
        val newEnv = Extend(x, v1, env)                            // Extend the environment with v1 bound to x

        /*- Evaluate e2 -*/
        val (v2, s3) = evalExpr(e2, newEnv, s2)                   // eval e2 to v2 under the environment newEnv
        val s4: Store =
          v2 match {
            case Reference(addr) => decrementReference(s1, addr)  // v2 is a Reference: Decrement reference count
            case _ => s3                                          // v1 is not a Reference: return the store
          }
        (v2, s4)                                                  // Return v2 and the final store s4
      }

      case Newref(e1) => {
        val (v1, s1) = evalExpr(e1, env, s)
        val (s2, addr) = createNewCell(s1, v1)
        (Reference(addr), s2)
      }

      case Deref(e1) => {
        val (v1, s1) = evalExpr(e1, env, s)
        val v2 = v1 match {
          case Reference(addr) => lookup(s1, addr)
          case _ => throw new IllegalArgumentException("Deref a non reference value is not permitted")
        }
        (v2, s1)
      }

      case Assignref(e1, e2) => {
        val (v2, s1) = evalExpr(e2, env, s)
        val (v1, s2) = evalExpr(e1, env, s1)
        val s3 = v1 match {
          case Reference(addr) => assignCell(s2, addr, v2)
          case _ => throw new IllegalArgumentException("Assignref to a non reference value is not permitted")
        }
        (v2, s3)
      }
    }
  }
  /*--------------------------------------------------------------------------------------------------------------*/

  def main( args: Array[String] ): Unit = {
    /*
     * let x = newref(10) in
     * let y = newref( let z = newref(15) in
     *                   let dummy = assignref(z, deref(x) + 5) in
     *                      deref(z) + 20
     *               ) in
     *     deref(y)
     */
    val x = Ident("x")
    val y = Ident("y")
    val z = Ident("z")

    val prog = Let("x", Newref(Const(10)),
      Let("y", Newref( Let("z", Newref(Const(15)),
        Let("dummy", Assignref(z, Plus(Deref(x), Const(5))),
          Plus(Deref(z), Const(20))
        )
      )
      ),
        Deref(y)
      ))

    val v = evalExpr(prog, EmptyEnvironment, emptyStore)

    println(s"Program: $prog")
    println(s"Result: ${toNumber(v)}")
  }
}
/*----------------------------------------------------------------------------------------------------------------*/