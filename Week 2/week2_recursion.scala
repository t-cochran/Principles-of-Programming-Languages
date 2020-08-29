/** 
  * File: week2_recursion.scala 
  * 
  * Working through the material on recursion and induction.
  * 
  */
object recursivePractice {

    def main( args : Array[ String ] ) : Unit = {

      func_2( 10 );

      val test_factorial = factorial( 4 );
      println( test_factorial );

    }





    /**
     * A function that returns x! using recursion 
     */
    def factorial( x : Int ) : Int = {

      if ( x == 0 ) {

        return 1;

      }
      else {

        return x * factorial( x - 1 );

      }

    }

    /**
     * A scala function using a while loop. 
     */
    def func_1( x : Int ) : Int = {

      var y = x;

      while( y >= 0 ) {

        println( y );

        y = y - 1;

      }

      return 0;

    }

    /**
     * func_1 using recursion 
     */
    def func_2( x : Int ) : Int = {

      if ( x < 0 ) {

        return x;

      }

      println( x );
      
      return func_2( x - 1 );

    }

      
}