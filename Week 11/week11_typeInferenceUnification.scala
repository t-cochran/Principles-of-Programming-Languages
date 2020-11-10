/*
 *  Week 11 - Type inference using unification
 */
sealed trait Program
case class TopLevel(e: Expr) extends Program
/*-------------------------------------------------------------------------------------------------------------------*/

sealed trait Expr
case class Const(f: Double) extends Expr
case class Ident(s: String) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class Geq(e1: Expr, e2: Expr) extends Expr
case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class Let(x: String, e1: Expr, e2: Expr) extends Expr
case class FunDef(id: String, e: Expr) extends Expr
case class FunCall(calledFun: Expr, argExpr: Expr) extends Expr
case class LetRec(funName: String, param: String, funExpr: Expr, bodyExpr: Expr) extends Expr
/*-------------------------------------------------------------------------------------------------------------------*/

sealed trait Type
case object NumType extends Type {
  override def toString: String = "num"
}
case object BoolType extends Type {
  override def toString: String = "bool"
}
case class FunType(t1: Type, t2: Type) extends Type {
  override def toString: String = s"($t1 => $t2)"
}
case class TypeVar(j: Int) extends Type {
  override def toString: String = s"t$j"
}
case class TypeEquation(t1: Type, t2: Type)
/*-------------------------------------------------------------------------------------------------------------------*/

case class TypeConstraints() {
  var nTypeVars: Int = 0               // Number of type vars currently in use
  var l: List[TypeEquation] = List()   // List of type equations, eg (t1, NumType) means t1 == NumType

  def createFreshVar(): TypeVar = {
    nTypeVars = nTypeVars + 1          // Increment total typevars
    TypeVar(this.nTypeVars)            // Create and return a new typevar
  }

  def addTypeEquation(t1: Type, t2: Type): Unit = {
    if (t1 == t2){
      println(s"Debug: trivial type equation $t1 == $t2 suppressed.")
    } else {
      l = TypeEquation(t1, t2)::l      // Add a type equation to the list of type equations
    }
  }

  def printAllTypeEquations(): Unit = {
    for (te <- l) {
      te match {
        case TypeEquation(t1, t2) => println(s"$t1 == $t2")
      }
    }
  }
}
/*-------------------------------------------------------------------------------------------------------------------*/

object getEquations {

  /**
   * Generate Type Variables
   *
   *    -> Traverse the program's AST and recursively generate new type variables
   *    -> tCons: List of equations
   *    -> This is 'generateEquations' from week 10 and 'step 2' in the course readings
   */
  def generateTypeVarsAndConstraints(e: Expr, alpha: Map[String, Type], tCons: TypeConstraints): Type = {

    /*- Helper: Given two expressions, get type equations and add them to the equation list -*/
    def handleSubExprs(e1: Expr, e2: Expr, operandType: Type, resultType: Type): Type = {
      val t1 = generateTypeVarsAndConstraints(e1, alpha, tCons)  // Get type equations for e1 and e2
      val t2 = generateTypeVarsAndConstraints(e2, alpha, tCons)
      tCons.addTypeEquation(t1, operandType)  // Add type equations to the equation list
      tCons.addTypeEquation(t2, operandType)
      resultType
    }

    /*- Pattern match expressions in the program AST, evaluate their type, build the equation list -*/
    e match {

      /*- Constants are NumType and Identifiers require an env lookup -*/
      case Const(_) => NumType
      case Ident(s) => if (alpha contains s)
        alpha(s)
      else
        throw new IllegalArgumentException(s"Unknown identifier: $s")

      /* Get types from these binary ops using the helper function 'handleSubExprs' -*/
      case Minus(e1, e2) => handleSubExprs(e1, e2, NumType, NumType)
      case Plus(e1, e2) => handleSubExprs(e1, e2, NumType, NumType)
      case Mult(e1, e2) => handleSubExprs(e1, e2, NumType, NumType)
      case Geq(e1, e2) => handleSubExprs(e1, e2, NumType, BoolType)
      case Eq(e1, e2) =>
        val t1 = generateTypeVarsAndConstraints(e1, alpha, tCons)
        val t2 = generateTypeVarsAndConstraints(e2, alpha, tCons)
        tCons.addTypeEquation(t1, t2)
        BoolType
      case IfThenElse(e, e1, e2) =>
        val t = generateTypeVarsAndConstraints(e, alpha, tCons)    // If boolean expr
        tCons.addTypeEquation(t, BoolType)
        val t1 = generateTypeVarsAndConstraints(e1, alpha, tCons)  // Then expr
        val t2 = generateTypeVarsAndConstraints(e2, alpha, tCons)  // Else expr
        tCons.addTypeEquation(t1, t2)
        t1

      /*- Let bindings -*/
      case Let(x, e1, e2) =>
        val t1 = generateTypeVarsAndConstraints(e1, alpha, tCons)   // Get e1 type
        val newEnv = alpha + (x -> t1)                              // Bind e1 to 'x'
        generateTypeVarsAndConstraints(e2, newEnv, tCons)           // Get e2 type

      /*- Function calls -*/
      case FunCall(e1, e2) =>
        val t = generateTypeVarsAndConstraints(e1, alpha, tCons)    // Get e1 type
        t match {
          /*- Explicit Func Call  -*/
          case FunType(t1Hat, t2Hat) => {     // Overall type is known: t1Hat => t2Hat
            val t1 = generateTypeVarsAndConstraints(e2, alpha, tCons)  // Get the argument type e2
            tCons.addTypeEquation(t1, t1Hat)  // Add equation: f( t1 ) == f( t1Hat )
            t2Hat
          }
          /*- TypeVar -*/
          case TypeVar(j) => {
            val nVar = tCons.createFreshVar()   // Create a type variable  for the result type
            val t1 = generateTypeVarsAndConstraints(e2, alpha, tCons)  // Get the argument type e2
            val t3 = FunType(t1, nVar)          // Overall type: t1 => nVar
            tCons.addTypeEquation(t, t3)        // Add equation: t == t1 => nVar
            nVar
          }
          case _ => throw new IllegalArgumentException(s"Type inference error: function call on non function type $t")
        }

      /*- Function definitions -*/
      case FunDef(x, e1) =>
        val tx = tCons.createFreshVar()  // Create a type variable for formal variable type x
        val newEnv = alpha + (x -> tx)   // Map formal variable x to its placeholder type
        val t2 = generateTypeVarsAndConstraints(e1, newEnv, tCons)  // Get the type the function body e1
        FunType(tx, t2)  // Overall type: tx => t2

      /*- Recursive functions -*/
      case LetRec(funName, param, funExpr, bodyExpr) =>
        val tx = tCons.createFreshVar()    // Create a type variable for the parameter type
        val tfun = tCons.createFreshVar()  // Create a type variable for the function type
        val newEnv = alpha ++ List(param -> tx, funName -> tfun)  // Update environment with param and function types
        val t2 = generateTypeVarsAndConstraints(funExpr, newEnv, tCons)  // Get the type the function body funExpr
        tCons.addTypeEquation(FunType(tx, t2), tfun)  // Add equation: paramT => bodyT == functionT
        tfun
    }
  }
}
/*-------------------------------------------------------------------------------------------------------------------*/

object Notes {
  /**
   *  Type inference review:
   *
   *    (1) Make a type variable for every subexpression in the program
   *    (2) Generate equations for each type variable
   *    (3) Solve equations using unification
   *
   *    let f: ??? function( x: ??? ) {    //  f ~> tf --- function(..) {..} ~> tr
   *                  x >= 30              //  x ~> tx --- x >= 30 ~> bool
   *              } in
   *       f( 10 ) + 25                    // f(10)+25 ~> num --- f(10) ~> tc
   *
   *
   */
  def main( args: Array[ String ] ): Unit = {

    /*- Print out the list of equations from a program -*/
    def generateAllEquations(p: Program): TypeConstraints = p match {
      case TopLevel(e) => {
        val tCons = new TypeConstraints()
        val emptyEnv: Map[String, Type] = Map()
        val t = getEquations.generateTypeVarsAndConstraints(e, emptyEnv, tCons)
        println("Equations: ")
        tCons.printAllTypeEquations()
        println("------------- ")
        println(s"If equations are solved then the program will have type : $t")
        tCons
      }
    }
    /*---------------------------------------------------------------------------------------------------------------*/

    // Program 1: print equations
    val p1 = TopLevel(
      Let( "x",                                       // let x = 15 in
        Const(15), Plus( Ident( "x" ), Const( 35 ) )    //    x + 35
        )
      )
    val equations1 = generateAllEquations(p1)
    /*---------------------------------------------------------------------------------------------------------------*/

    // Program 2: print equations
    val p2 = TopLevel(
      Let( "f",                                                       // let f = function(x) {
        FunDef( "x", Geq( Ident( "x" ), Const( 35 ) ) ),              //            x >= 35
        Plus( FunCall( Ident( "f" ), Const( 20 ) ), Const( 35 ) ) )   //        } in
      )                                                               //    f( 20 ) + 35
    val equations2 = generateAllEquations(p2)
    /*---------------------------------------------------------------------------------------------------------------*/

    // Program 3: print equations
    val p3 = TopLevel(                                        // let f = function(x) {
      Let( "f",                                               //          x + x
        FunDef( "x", Plus( Ident( "x" ), Ident( "x" ) ) ),    //        } in
        FunCall( Ident( "f" ), Ident( "f" ) ) )               //    f( f )
    )
    val equations3 = generateAllEquations(p3)
    /*---------------------------------------------------------------------------------------------------------------*/

    // Program 4: print equations
    val p4 = TopLevel(
      Let( "x",
        Const( 15 ),
        Let( "y",
          Plus( Ident( "x" ), Const( 30 ) ), Plus( Ident( "x" ), Ident( "y" ) ) ) )
    )
    val equations4 = generateAllEquations(p4)
    /*---------------------------------------------------------------------------------------------------------------*/
  }
}