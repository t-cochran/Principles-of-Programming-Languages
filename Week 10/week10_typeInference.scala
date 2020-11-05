/**
 *  Week 10 - Covering course material type annotations, type checking, and type inference
 */

/*- Type annotations: Numbers, Booleans, Functions -*/
sealed trait Type
case object NumType extends Type
case object BoolType extends Type
case class FunType(t1: Type, t2: Type) extends Type
case class TypeErrorException(s: String) extends Exception
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
case class Let(x: String, xType: Type, e1: Expr, e2: Expr) extends Expr   // Let bindings type annotation
case class FunDef(id: String, idType: Type, e: Expr) extends Expr         // Function definition type annotation
case class FunCall(calledFun: Expr, argExpr: Expr) extends Expr
case class LetRec(funName: String, funType: Type,
                  param: String, paramType: Type,
                  funExpr: Expr, bodyExpr: Expr) extends Expr             // Recursive function type annotation
/*-------------------------------------------------------------------------------------------------------------------*/

sealed trait Program
case class TopLevel(e: Expr) extends Program
/*-------------------------------------------------------------------------------------------------------------------*/

/**
 *  Type checker: Determine whether a program is well typed or not
 *    -> In: Top level expression for a program
 *    -> Out: Program's type (well typed) or error (mistyped)
 */
object TypeChecker {

  /*- TypeOf: like an eval function, but:
   *    (1) Returns types
   *    (2) Uses an environment that maps Identifiers to Types
   */
  def typeOf(e: Expr, alpha: Map[String, Type]): Type = {

    /**
     *  checkType:
     *      Recursively evaluate expressions e1 and e2, ensuring that each expression equals the
     *      intended types t1 and t2
     */
    def checkType( opName: String, e1: Expr, t1: Type, e2: Expr, t2: Type, resType: Type): Type = {
      if (!(typeOf(e1, alpha) == t1)) {
        throw TypeErrorException(s"Type mismatch in op $opName, Expected type $t1, obtained ${typeOf(e1, alpha)}")
      }
      if (!(typeOf(e2, alpha) == t2)) {
        throw TypeErrorException(s"Type mismatch in op $opName, Expected type $t2, obtained ${typeOf(e2, alpha)}")
      }
      resType
    }

    /**
     * 'e match':
     *    (1) Pattern match expressions (like an eval function)
     *    (2) Check for type errors by calling 'checkType'
     */
    e match {
      case Const(f) => NumType
      case Ident(s) =>
        if (alpha contains s)
          alpha(s)
        else
          throw TypeErrorException(s"Unknown identifier $s")

      case Plus(e1, e2) =>  checkType(opName="Plus", e1, NumType, e2, NumType, NumType)
      case Minus(e1, e2) => checkType(opName="Minus",e1, NumType, e2, NumType, NumType)
      case Mult(e1, e2) => checkType(opName="Mult",e1, NumType, e2, NumType, NumType)
      case Div(e1, e2) => checkType(opName="Div", e1, NumType, e2, NumType, NumType)
      case Geq(e1, e2) => checkType(opName="Geq", e1, NumType, e2, NumType, BoolType)
      case Eq(e1, e2) =>
        val t1 = typeOf(e1, alpha)
        val t2 = typeOf(e2, alpha)
        if (t1 == t2)
          BoolType
        else
          throw TypeErrorException(s"Equality operator: unequal types $t1 with $t2")

      case IfThenElse(e, e1, e2) =>
        val t = typeOf(e, alpha)
        if (t == BoolType){             // If(e): Check whether 'e' is of Booltype
          val t1 = typeOf(e1, alpha)    // then(e1)
          val t2 = typeOf(e2, alpha)    // else(e2)
          if (t1 == t2)
            t1
          else
            throw TypeErrorException(s"If then else returns unequal types $t1 and $t2")
        } else {
          throw TypeErrorException(s"If then else condition expression not boolean $t")
        }

      case Let(x, t, e1, e2) =>
        val t1 = typeOf(e1, alpha)
        if (t1 == t) {                     // Check if the expression 'e1' has type 't' of bound identifier 'x'
          typeOf(e2, alpha + (x -> t))     // Expand the type environment with 'x' bound to type 't' of identifer 'x'
        } else {
          throw TypeErrorException(s"Let binding has type $t whereas it is bound to expression of type $t1")
        }

      case FunDef(x, t1, e) =>
        val newAlpha = alpha + (x -> t1)
        val t2 = typeOf(e, newAlpha)
        FunType(t1, t2)

      case FunCall(e1, e2) =>
        val ftype = typeOf(e1, alpha)
        ftype match {
          case FunType(t1, t2) =>
            val argType = typeOf(e2, alpha)
            if (argType == t1){
              t2
            } else {
              throw TypeErrorException(s"Function has Incompatible argument type. Expected $t1, obtained $argType")
            }
          case _ => throw TypeErrorException(s"FunCall: Non function type $ftype")

        }

      case LetRec(f, fType, x, argType, e1, e2 ) =>
        fType match {
          case FunType(t1, t2) =>
            if (argType == t1){
              val newAlpha = alpha + (f -> fType, x -> argType)
              val retType = typeOf(e1, newAlpha)
              if (retType == t2){
                typeOf(e2, alpha + (f -> fType))
              } else {
                throw TypeErrorException(s"Recursive call expression evaluates to type $retType. Expected type: $t2")
              }
            } else {
              throw TypeErrorException(s"Incompatible recursive function type $fType with arg type: $argType")
            }
          case _ => throw TypeErrorException(s"LetRec: Non function type: $fType")
        }
    }
  }
  /*-----------------------------------------------------------------------------------------------------------------*/

  def typeCheck(p: Program): Unit = p match {
    case TopLevel(e) =>
      try {
        val t = typeOf(e, Map())
        println(s"Program type computed successfully as $t")
      } catch {
        case TypeErrorException(s) => println(s"Type error found: $s")
      }
  }
  /*-----------------------------------------------------------------------------------------------------------------*/

  def main(args: Array[String]): Unit = {

    // Type checker: Test 1 -- Well Typed
    val p1 = TopLevel(
      Let("x", NumType, Const(20),     // let x : NumType = 20 in
        Plus(Ident("x"), Const(30))    //    x + 30
      )
    )
    typeCheck(p1)
    /*---------------------------------------------------------------------------------------------------------------*/

    // Type checker: Test 2 -- Mistyped
    val p2 = TopLevel(                                    // let y : Numtype = 15 in
      Let("y", NumType, Const(15),                        //    let x : BoolType = 25 >= y in
        Let("x", BoolType, Geq (Const(25), Ident("y")),   //        x >= (30 >= y)
          Geq( Ident("x"), Geq(Const(30), Ident("y")) )
        )
      )
    )
    typeCheck(p2)
    /*---------------------------------------------------------------------------------------------------------------*/

    // Type checker: Test 3 -- Well Typed
    val p3 = TopLevel(
      Let("f", FunType( FunType(NumType, NumType), NumType ),                      // let f: (Num => Num) => Num =
        FunDef("g", FunType(NumType, NumType), FunCall( Ident("g"), Const(20) )),  //     function(g: Num => Num) {
          Let("g", FunType(NumType, NumType),                                      //         g(20)
            FunDef("x", NumType, Ident("x")), FunCall(Ident("f"), Ident("g"))      //     } in
          )                                                                        //     let g: Num => Num =
      )                                                                            //       function(x: Num) {
    )                                                                              //         x
    typeCheck(p3)                                                                  //       } in
                                                                                   //           f( g )
    /*---------------------------------------------------------------------------------------------------------------*/

    // Type checker: Test 4 -- Well Typed
    val p4 = TopLevel(
      LetRec("f", FunType(NumType, NumType), "z", NumType,      //  let rec f: Num => Num = function(z: Num) {
        IfThenElse(                                             //      if (z <= 0)
          Geq(Const(0), Ident("z")),                            //      then 1
          Const(1),                                             //      else 1 + f(z-1)
          Plus(Const(1),                                        //    in
            FunCall(Ident("f"),                                 //      f(10)
              Minus(Ident("z"), Const(1))
            )
          )
        ),
        FunCall(Ident("f"), Const(10))
      )

    )
    typeCheck(p4)
  }
  /*-----------------------------------------------------------------------------------------------------------------*/
}