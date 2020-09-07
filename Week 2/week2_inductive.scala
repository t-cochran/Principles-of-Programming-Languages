/** 
  * File: week2_inductive.scala 
  * 
  * Working through the material on inductively defined structures.
  * 
  * NOTES: 
  * Use the grammars to generate terms.
  * A tree version of a term can be "flattened" into a nested set of rules
  * by performing a pre-order traversal.
  */

/**
  * Grammar for natural numbers:
  *  
  * Number -> Z | Succ( Number )
  * 
  * { ∅ / 0 } U { { n }/n+1 | n ∈ ℕ } 
  */
sealed trait Number;
/* --------------------------------------------------------------- */

/**
  * Grammar for a list of numbers:
  * 
  * Num = 0 | 1 | 2 | 3 . . .
  * NumList -> Nil | Const( Num, NumList )
  * 
  */
sealed trait NumList;
/* --------------------------------------------------------------- */

/**
  * Grammar for a binary tree of numbers:
  * 
  * Num = 0 | 1 | 2 | 3 . . .
  * NumTree -> Leaf | Node( Num, NumTree, NumTree ) 
  */
sealed trait NumTree;
/* --------------------------------------------------------------- */

/**
  * Grammar for arithmetic expressions: 
  * 
  * Double -> -2 | -1 | 0 | 1 | 2 . . .
  * Identifier -> [a-zA-Z][a-zA-Z0-9_]
  * Expr -> Const( Double )
  *     |   Ident( Identifier )
  *     |   Plus( Expr, Expr )
  *     |   Minus( Expr, Expr )
  *     |   Mult( Expr, Expr )
  *     |   Div( Expr, Expr )
  *     |   Log( Expr )
  *     |   Exp( Expr )
  *     |   Sine( Expr )
  *     |   Cosine( Expr )
  */
sealed trait Expr;
/* --------------------------------------------------------------- */

/**
  * Grammar for conditional expressions: 
  * 
  * CondExpr -> ConstTrue
  *         |   ConstFalse
  *         |   Geq( Expr, Expr )
  *         |   Leq( Expr, Expr )
  *         |   Eq( Expr, Expr )
  *         |   And( CondExpr, CondExpr )
  *         |   Or( CondExpr, CondExpr )
  *         |   Not( CondExpr )
  * 
  * Double -> -2 | -1 | 0 | 1 | 2 . . .
  * Identifier -> [a-zA-Z][a-zA-Z0-9_]*
  * Expr -> Const( Double )
  *     |   Ident( Identifier )
  *     |   Plus( Expr, Expr )
  *     |   Minus( Expr, Expr )
  *     |   Mult( Expr, Expr )
  *     |   Div( Expr, Expr )
  *     |   Log( Expr )
  *     |   Exp( Expr )
  *     |   Sine( Expr )
  *     |   Cosine( Expr )
  */
sealed trait CondExpr;
/* --------------------------------------------------------------- */


object inductiveStructs {

  /* Inductive definition for the set of natural numbers */
  case class Z() extends Number;                  // Number -> Z i.e. 0
  case class Succ( n : Number ) extends Number;   // Number -> Succ( Number ) i.e. n + 1

  /* --------------------------------------------------------------- */
  
  /* Inductive definition for a list of numbers */
  case object Nil extends NumList;
  case class Cons( n : Int, lst : NumList ) extends NumList;

  /* --------------------------------------------------------------- */

  /* Inductive definition for a binary tree of numbers */
  case object Leaf extends NumTree;
  case class Node( n : Int, left : NumTree, right : NumTree ) extends NumTree;

  /* --------------------------------------------------------------- */

  /* Inductive definition for arithmetic expressions */
  case class Const( d : Double ) extends Expr;            // 2, 34, 13, 10
  case class Ident( s : String ) extends Expr;            // "x", "x_1", "y", "velocity"
  case class Plus( e1 : Expr, e2 : Expr ) extends Expr;   // +
  case class Minus( e1 : Expr, e2 : Expr ) extends Expr;  // -
  case class Mult( e1 : Expr, e2 : Expr ) extends Expr;   // *
  case class Div( e1 : Expr, e2 : Expr ) extends Expr;    // /
  case class Log( e1 : Expr ) extends Expr;               // log
  case class Exp( e1 : Expr ) extends Expr;               // ^
  case class Sine( e1 : Expr ) extends Expr;              // sin
  case class Cosine( e1 : Expr ) extends Expr;            // cos

  /* --------------------------------------------------------------- */

  /* Inductive definition for conditional expressions */
  case object ConstTrue extends CondExpr;                          // true
  case object ConstFalse extends CondExpr;                         // false
  case class Geq( e1: Expr, e2 : Expr ) extends CondExpr;          // >=
  case class Leq( e1: Expr, e2 : Expr ) extends CondExpr;          // <=
  case class Eq( e1: Expr, e2 : Expr ) extends CondExpr;           // ==
  case class And( e1: CondExpr, e2 : CondExpr ) extends CondExpr;  // ^
  case class Or( e1: CondExpr, e2 : CondExpr ) extends CondExpr;   // v
  case class Not( e1: CondExpr, e2 : CondExpr ) extends CondExpr;  // ~

  /* --------------------------------------------------------------- */

  def main( args : Array[ String ] ) : Unit = {

    /* Using the inductive definition of natural numbers to get n = 5 */
    val zero = Z();                                            // ℕ = 0
    val five = Succ( Succ( Succ( Succ( Succ( zero ) ) ) ) );   // ℕ = 0, 1, 2, 3, 4, 5
    val seven = Succ( Succ( five ) );                          // ℕ = 0, 1, 2, 3, 4, 5, 6, 7
    println( five );
    println( seven );
    println( Succ( Succ( five ) ) ==  Succ( 
                                        Succ( 
                                          Succ( 
                                            Succ( 
                                              Succ( 
                                                Succ( 
                                                  Succ( 
                                                    zero 
                                                  ) 
                                                ) 
                                              )
                                            ) 
                                          ) 
                                        ) 
                                      ) 
                                    );

  /* --------------------------------------------------------------- */

    /* Using the inductive definition of a list of numbers */
    val list_1 = Cons( 3, Nil );             // List( 3 )
    val list_2 = Cons( 2, Cons( 3, Nil ) );  // List( 3, 2 )
    val list_3 = Cons( 2, list_1 );          // List( 3, 2 )
    println( list_1 == list_2 );             // false
    println( list_2 == list_3 );             // true

  /* --------------------------------------------------------------- */

    /* Using the inductive definition of a binary tree of numbers */
    val tree_1 = Node( 5, Leaf, Leaf );
    val tree_2 = Node( 10, Leaf, tree_1 );
    val tree_3 = Node( 10, Leaf, Node( 5, Leaf, Leaf ) );
    println( tree_2 == tree_3 );

  /* --------------------------------------------------------------- */

    /* Using the inductive definition of arithmetic expressions */
    val exp_1 = Plus( Mult( Ident( "x" ), Ident( "x" ) ), Const( 10 ) );  // x^2 + 10
    val exp_2 = Plus( Sine( Ident( "x" ) ), Cosine( Ident( "x" ) ) );     // sine(x) + cosine(y)
    println( exp_1 );
    println( exp_2 );

  /* --------------------------------------------------------------- */

    /* Using the inductive definition of conditional expressions */
    val condExp_1 =  Geq( 
                      Mult( 
                        Ident( "x" ), 
                        Ident( "x" ) 
                      ), 
                      Plus( 
                        Ident( "y" ), 
                        Const( 3 ) ) 
                      );  // x^2 >= y + 3

    val condExp_2 = And( 
                      Geq( 
                        Mult( 
                          Ident( "x" ), 
                          Ident( "x" ) 
                        ), 
                        Plus( 
                          Ident( "y" ), 
                          Const( 3 ) 
                          ) 
                      ),
                      Leq(
                        Plus( 
                          Ident( "x" ), 
                          Minus( 
                            Ident( "y" ),
                            Exp( 
                              Ident( "z" ) 
                            )
                          ),
                        ),
                        Const( 0 )
                      )
                    );  // x^2 >= y + 3 ^ x + y - e^z <= 0
    println( condExp_1 );
    println( condExp_2 );

  /* --------------------------------------------------------------- */

  }

}