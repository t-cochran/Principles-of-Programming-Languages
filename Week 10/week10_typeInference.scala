/**
 *  Week 10 - Covering course material type annotations, type checking, and type inference
 */

/*- Type annotations: Numbers, Booleans, Functions -*/
sealed trait Type
case object NumType extends Type
case object BoolType extends Type
case class FunType(t1: Type, t2: Type) extends Type
/*-------------------------------------------------------------------------------------------------------------------*/

sealed trait Expr
case class Const(f: Double) extends Expr
case class Ident(s: String) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class Geq(e1: Expr, e2: Expr) extends Expr
case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class Let(x: String, xType: Type, e1: Expr, e2: Expr) extends Expr
case class FunDef(id: String, idType: Type, e: Expr) extends Expr
case class FunCall(calledFun: Expr, argExpr: Expr) extends Expr
case class LetRec(funName: String, funType: Type, param: String, paramType: Type, funExpr: Expr, bodyExpr: Expr) extends Expr
/*-------------------------------------------------------------------------------------------------------------------*/

sealed trait Program
case class TopLevel(e: Expr) extends Program
/*-------------------------------------------------------------------------------------------------------------------*/

object Notes{
  def sanityTest(): Unit = {
    println("scala code from the week 10 sbt");
  }
  def main(args: Array[String]): Unit = {
    sanityTest()
  }
}