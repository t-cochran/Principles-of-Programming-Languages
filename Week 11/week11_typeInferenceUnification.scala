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

case class SolverError(msg: String) extends Exception
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
/**
 *  Type inference review:
 *
 *    (1) Make a type variable for every subexpression in the program
 *    (2) Generate equations for each type variable
 */
object getEquations {

  /**
   * Generate Type Variables, i.e. generate constraints
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

/**
 *  Solving type equations by substitution on the list of type equations:
 *
 *    let f: ??? function( x: ??? ) {    //  [f ~> tf], [function(..) {..} ~> tr]
 *                  x >= 30              //  [x ~> tx], [x >= 30 ~> bool]
 *              } in
 *       f( 10 ) + 25                    // [f(10)+25 ~> num], [f(10) ~> tc]
 *
 *    Use substitution to solve the unknown types (tf, tr, tx, tc) systematically.
 *    Substitution data structure: Map[ Type -> Constraint ]
 */
object substitutionHelpers {

  /* Helper: Given Type 'tExpr', return true if it contains a type var */
  def typeExprContainsVariable(tExpr: Type, tVar: TypeVar): Boolean = {
    tExpr match {
      case FunType(t1, t2) =>
        typeExprContainsVariable(t1, tVar) || typeExprContainsVariable(t2, tVar) // Recursively search for type vars
      case TypeVar(_) => tExpr == tVar
      case _ => false
    }
  }

  /**
   * Return a substitution for a type expression (tExpr) by indexing a Mapping between [TypeVar |--> Type]
   * If the mapping contains the typevar for the expression, then the substitution 'Type' is returned
   *
   * EX:
   * (1) tExpr is type: (t1 => t2) => num
   * (2) subst for t2 found! It is: t2 |---> (num => num)
   * (3) Return: (t1 => (num => num)) => num
   *
   * Given a type expression (tExpr), return the type that it substitutes for by indexing: TypeVar |--> Type
   */
  def substituteExpr(tExpr: Type, subst: Map[TypeVar, Type]): Type = {
    tExpr match {

      /*- Type: TypeVar() index Map[TypeVar|-->Type], return Type -*/
      case TypeVar(j) =>
        if (subst contains TypeVar(j))
          subst(TypeVar(j))
        else
          tExpr // No substitute exists, so you are left with the type expression you have

      /*- Type: t1 => t2, recursively index Map[TypeVar|-->Type], return Type -*/
      case FunType(t1, t2) =>
        FunType(substituteExpr(t1, subst), substituteExpr(t2, subst))

      /*- No substitute exists -*/
      case _ =>
        tExpr
    }
  }

  /**
   * Update the substitution map to contain a new rule 'tVar -> Type'
   *
   * If we do this, we must substitute type expressions that match the rule on the right hand side, for example:
   *
   *    t1 |-> t2 => t3                ADD NEW RULE: t2 |-> num => t3
   *    t4 |-> num => num
   *
   *    t1 |-> (num => t3) => t3       Substitute t2 on the RHS with the new rule
   *    t4 |-> num => num
   *
   *    t1 |-> (num => t3) => t3
   *    t4 |-> num => num
   *    t2 |-> num => t3              Include the new rule at the end
   *
   * At the end, we include the new rule
   */
  def updateSubstitutionWithNewRule(tVar: TypeVar, tExpr: Type, subst: Map[TypeVar, Type]): Map[TypeVar, Type] = {
    assert(!subst.contains(tVar), s"Error: Substitution already contains the type variable $tVar")
    assert(!typeExprContainsVariable(tExpr, tVar), "Substitution RHS cannot contain LHS variable")
    val newSubst = subst.foldLeft[Map[TypeVar, Type]]( Map[TypeVar, Type]() ) {  // fold through each equation listed
      case (m, (t, te)) =>
        m + (t -> substituteExpr(te, Map(tVar -> tExpr)))  // Substitute tVar on the RHS if a substitute can occur
    }
    newSubst + (tVar -> tExpr)  // Add the new substitution rule 'tVar |-> tExpr' at the very end
  }
}
/*-------------------------------------------------------------------------------------------------------------------*/

object substitutionAlgorithm {

  /*- Helper: Return true if 't' is Type: FunType -*/
  def isFunctionType(t: Type) = {
    t match {
      case FunType(_,_) => true
      case _ => false
    }
  }

  /**
   *  Given:
   *      (1) A type equation 'teq'
   *      (2) A ist of type equations (i.e. substitutions) 'subst'
   *  Apply substitutions to the type equation and return an updated list of type equations
   */
  def processEquation(teq: TypeEquation, subst: Map[TypeVar, Type], msg: String = ""): Map[TypeVar, Type] =  {

    /*- Apply all substitutions to the type equation 'teq' to get the overall types -*/
    val (t1, t2) = {
      teq match {
        case TypeEquation(t1Hat, t2Hat) =>
          ( substitutionHelpers.substituteExpr(t1Hat, subst), substitutionHelpers.substituteExpr(t2Hat, subst) )
      }
    }

    /*- LHC == RHC: Done; return the updated list of type equations -*/
    if (t1 == t2) {
      subst
    }

    /*- LHC != RHC: Perform unification -*/
    else {

      (t1, t2) match {

        /*- t1, t2 cannot be unified --------------------------------------------------------------------------------*/
        case (NumType, BoolType) => {
          throw new SolverError( msg=s"@ $msg: Numerical and Boolean types used interchangably")
        }
        case (BoolType, NumType) => {
          throw new SolverError(msg=s"@ $msg: Numerical and Boolean types used interchangably")
        }
        case (NumType, tf) if isFunctionType(tf) => {
          throw new SolverError( msg=s"@ $msg: Numerical and Function types used interchangably")
        }
        case (tf, NumType) if isFunctionType(tf) => {
          throw new SolverError(msg=s"@ $msg: Numerical and Function types used interchangably")
        }
        case (BoolType, tf) if isFunctionType(tf) => {
          throw new SolverError(msg=s"@ $msg: Numerical and Function types used interchangably")
        }
        case (tf, BoolType) if isFunctionType(tf) => {
          throw new SolverError(msg=s"@ $msg: Numerical and Function types used interchangably")
        }

        /*- t1, t2 can be unified -----------------------------------------------------------------------------------*/
        case (TypeVar(j), _ ) => {  // t1 is a TypeVar
          if (substitutionHelpers.typeExprContainsVariable(t2, TypeVar(j))){
            throw new SolverError(msg=s"@ $msg: Type variable also in the RHS of an equation: no solution can exist.")
          }
          substitutionHelpers.updateSubstitutionWithNewRule(TypeVar(j), t2, subst)  // Add TypeVar(j) |-> t2
        }
        case (_, TypeVar(j)) => {  // t2 is a TypeVar
          assert(t1 != TypeVar(j))
          if (substitutionHelpers.typeExprContainsVariable(t1, TypeVar(j))){
            throw new SolverError(msg=s"@ $msg: Type variable also in the RHS of an equation: no solution can exist.")
          }
          substitutionHelpers.updateSubstitutionWithNewRule(TypeVar(j), t1, subst)  // Add TypeVar(j) |-> t1
        }
        case (FunType(t1Hat, t2Hat), FunType(t3Hat, t4Hat)) => {  // t1 and t2 are FunTypes
          val subst1 = processEquation(TypeEquation(t1Hat, t3Hat), subst, msg)   // Apply substitutions to func1
          val subst2 = processEquation(TypeEquation(t2Hat, t4Hat), subst1, msg)  // Apply substitutions to func2
          subst2
        }
        case _ => {
          throw new SolverError(msg=s"@ $msg:Cannot unify disparate types $t1, $t2")
        }
      }
    }
  }
  /*-----------------------------------------------------------------------------------------------------------------*/

  def processAllEquations(tCons: TypeConstraints): Unit = {
    println("Equations: ")
    tCons.printAllTypeEquations()
    println("Solving:")
    val finalSubst = tCons.l.foldLeft[Map[TypeVar, Type]] (Map()) {
      case (subst, te1) => processEquation(te1, subst)
    }
    println(s"Solution: $finalSubst")
  }
}

/*-------------------------------------------------------------------------------------------------------------------*/
object Notes {

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
      Let( "x",                                         // let x = 15 in
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
      Let( "x",                                       // let x = 15 in
        Const( 15 ),                                  //    let y = x + 30 in
        Let( "y",                                     //        x + y
          Plus( Ident( "x" ), Const( 30 ) ),
          Plus( Ident( "x" ), Ident( "y" ) ) ) )
    )
    val equations4 = generateAllEquations(p4)
    /*---------------------------------------------------------------------------------------------------------------*/
  }
}