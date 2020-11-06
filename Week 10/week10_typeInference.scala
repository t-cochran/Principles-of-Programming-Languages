/**
 *  Week 10 - Covering course material type inference
 */
object TypeInference {

  /*- Expressions -*/
  sealed trait Expr
  case class Const(f: Double) extends Expr
  case class Ident(x: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Geq (e1: Expr, e2: Expr) extends Expr
  case class And(e1: Expr, e2: Expr) extends Expr
  case class Not(e: Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr
  case class Let(x: String, e1: Expr, e2: Expr) extends Expr
  case class FunDef(param: String, body: Expr) extends Expr
  case class FunCall(e1: Expr, e2: Expr) extends Expr
  case class LetRec(f: String, x: String, e1: Expr, e2: Expr ) extends Expr
  /*----------------------------------------------------------------------------------------------------------------*/

  /*- Type annotations -*/
  sealed trait Type
  case object NumType extends Type
  case object BoolType extends Type
  case class FunType(type1: Type, type2: Type) extends Type
  case class TypeVar(name:String) extends Type  // Handles unknown types

  /*- Type error -*/
  case class ErrorException(s: String) extends Exception
  /*----------------------------------------------------------------------------------------------------------------*/

  /*- Type environment -*/
  type TypeEnvironment = Map[String, Type]    // Env α: Identifiers -> Type

  /*- Type equations -*/
  type ListOfEquations = List[(Type, Type)]   // Tuples of expressions and their types

  /**
   *  TypeVarGenerator
   *      -> Set type variables for every expression in the program, e.g. { x -> typeX, y -> typeY, ... }
   *      -> Generate TypeVar strings (type_id_counter) for unknown types
   */
  object TypeVarGenerator {
    var counter = 0
    def getFreshTypeVariable(id: String): TypeVar = {
      val t = TypeVar("type_" + id + "_" + counter.toString)
      counter = counter + 1
      t
    }
    def resetCounter(): Unit = {
      counter = 0
    }
  }
  /*----------------------------------------------------------------------------------------------------------------*/

  /**
   *  generateEquations
   *      -> Systematically set up equations for each type variable
   *      -> Sorta like an eval function
   *            IN:  expression, type environment
   *            OUT: Tuple (Overall Type, List( (equation, Type) )
   */
  def generateEquations(e: Expr, alpha: TypeEnvironment): (Type, ListOfEquations) = {

    /**
     *  Pattern match expressions in the program AST to evaluate their overall type
     */
    e match {

      /*- Constants will always be NumType -*/
      case Const(f) => (NumType, Nil)

      /*- Identifiers are looked up from the type environment: α(x) if x ∈ domain(α) -*/
      case Ident(id) => {
        if (alpha contains id){  // Lookup the identifier in the type environment α
          (alpha(id), Nil)       // Return the Type mapped to the Identifier
        } else {
          throw new ErrorException(s"Used undeclared identifier $id -- type error")
        }
      }
      /*------------------------------------------------------------------------------------------------------------*/

      /*- Plus: Collect a list of type equations for each sub-expression; overall type is NumType -*/
      case Plus(e1, e2) => {
        val (t1, lst1) = generateEquations(e1, alpha) // Get overall (type, equations) for sub-expression e1
        val (t2, lst2) = generateEquations(e2, alpha) // Get overall (type, equations) for sub-expression e2
        val combinedList = lst1 ++ lst2 ++ List( (t1, NumType), (t2, NumType) )  // t1, t2 must be NumType
        (NumType, combinedList) // Plus() must be NumType, combineList is a list of all sub-expr equations
      }

      /*- Geq: Collect a list of type equations for each sub-expression; overall type is BoolType -*/
      case Geq(e1, e2) => {
        val (t1, lst1) = generateEquations(e1, alpha)
        val (t2, lst2) = generateEquations(e2, alpha)
        val combinedList = lst1 ++ lst2 ++ List( (t1, NumType), (t2, NumType) )
        (BoolType, combinedList)
      }

      /*- And: Collect a list of type equations for each sub-expression; overall type is BoolType -*/
      case And(e1, e2) => {
        val (t1, lst1) = generateEquations(e1, alpha)
        val (t2, lst2) = generateEquations(e2, alpha)
        val combinedList = lst1 ++ lst2 ++ List( (t1, BoolType), (t2, BoolType) )
        (BoolType, combinedList)
      }

      /*- Not: Collect a list of type equations for each sub-expression; overall type is BoolType -*/
      case Not(e1) => {
        val (t1, lst1) = generateEquations(e1, alpha)
        val combinedList = lst1 ++ List( (t1, BoolType) )
        (BoolType, combinedList)
      }

      /*- IfThenElse: Collect a list of type equations for each sub-expression; overall type is NumType -*/
      case IfThenElse(e, e1, e2) => {
        val (t0, lst0) = generateEquations(e, alpha)
        val (t1, lst1) = generateEquations(e1, alpha)
        val (t2, lst2) = generateEquations(e2, alpha)
        val combinedList = lst1 ++ lst2 ++ List( (t0, BoolType), (t1, t2) )
        (t1, combinedList)
      }
      /*------------------------------------------------------------------------------------------------------------*/

      case FunDef(param, body) => {
        val tparam = TypeVarGenerator.getFreshTypeVariable(param) // Generate a type for the function parameter
        val newEnv = alpha + (param -> tparam)                    // map the function parameter to the type
        val (tbody, listBody) = generateEquations(body, newEnv)   // Collect types for the function body expressions
        val fnType = FunType(tparam, tbody)                       // Set function type as ( Type param => Type body )
        (fnType, listBody)                                        // Return function type and list of type equations
      }

      case FunCall(e1, e2) => {
        val (te1, listE1) = generateEquations(e1, alpha)              // Type of function identifer
        val (te2, listE2) = generateEquations(e2, alpha)              // Type of function argument
        val te = TypeVarGenerator.getFreshTypeVariable(id="fcall")    // Generate a type for the return
        val newTypeConstraint = (te1, FunType(te2, te) )              // Set function type ( Type ident => Type func )
        val combinedList = listE1 ++ listE2 ++ List(newTypeConstraint)
        (te, combinedList)
      }

      case Let(x, e1, e2) => {
        val tx = TypeVarGenerator.getFreshTypeVariable(x)         // Generate a type for the identifier x
        val (te1, listE1) = generateEquations(e1, alpha)          // Type of expression bound to the identifier
        val newAlpha = alpha + (x -> tx)                          // Map the identifier x to its type tx
        val (te2, listE2) = generateEquations(e2, newAlpha)       // Type of expression after 'in'
        val combinedList = listE1 ++ listE2 ++ List( (tx, te1) )
        (te2, combinedList)                                       // Overall type is the type for e2
      }
    }
  }
  /*----------------------------------------------------------------------------------------------------------------*/

  // Helpers to print type equations generated by generateEquations
  def typeToString(t: Type) : String = t match {
    case NumType => "num"
    case BoolType => "bool"
    case FunType(t1, t2) => "("+(typeToString(t1)) + " => " + (typeToString(t2)) +")"
    case TypeVar(str) => str
  }

  def prettyPrintTypeEqs (lst: List[(Type, Type)]): Unit = {
    lst.foreach {
      case (t1, t2) => {
        println( typeToString(t1) + " == " + typeToString(t2) )
      }
    }
  }
  /*----------------------------------------------------------------------------------------------------------------*/

  def main(args: Array[String]): Unit = {
    // Test 1 -- generate type equations
    // Expected types: { f -> ( num => bool ), x -> num }
    val p1 = Let("f",                                   // let f = function(x) {
              FunDef("x", Geq(Ident("x"), Const(15))),  //            x >= 15
              FunCall(Ident("f"), Const(35)))           //        } in
                                                        //            f(35)

    val emptyTypeEnv = Map[String, Type]()    // Empty type environment
    TypeVarGenerator.resetCounter             // Empty typeVar counter

    val (typ, lstOfEqs) = try                 // Get the overall type of the program
      generateEquations(p1, emptyTypeEnv)
    catch {
      case ErrorException(msg) => { println(msg); throw new ErrorException(msg) }
    }
    println("-- Generated Eqs --")           // Print overall type of the program and list of type equations
    prettyPrintTypeEqs(lstOfEqs)
    println("The overall program has type: " + typeToString(typ))
    /*-------------------------------------------------------------------------------------------------------------*/

    // Test 2


  }
}
/*------------------------------------------------------------------------------------------------------------------*/