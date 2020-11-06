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
  /*-----------------------------------------------------------------------------------------------------------------*/

  /*- Type annotations -*/
  sealed trait Type
  case object NumType extends Type
  case object BoolType extends Type
  case class FunType(type1: Type, type2: Type) extends Type
  case class TypeVar(name:String) extends Type  // Handles unknown types
  /*-----------------------------------------------------------------------------------------------------------------*/

  /*- Type environment -*/
  type TypeEnvironment = Map[String, Type]    // Env α: Identifiers -> Type

  /*- Type equations -*/
  type ListOfEquations = List[(Type, Type)]   // Tuples of expressions and their types

  /*- Generate TypeVar strings (type_id_counter) for unknown types -*/
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
  /*-------------------------------------------------------------------------------------------------------------------*/

  def main(args: Array[String]): Unit = {
    println("TODO")
  }
}
/*-------------------------------------------------------------------------------------------------------------------*/