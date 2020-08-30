import scala.annotation.tailrec
/** 
  * File: week2_recursion.scala 
  * 
  * Working through the material on recursion and induction.
  * 
  */
object recursivePractice {

    def main( args : Array[ String ] ) : Unit = {

      // func_2( 10 );

      val test_factorial = factorial( 4 );
      // println( test_factorial );

      var check = fibonacci_rec_2( 5 );
      println( s"$check" );
      check = fibonacci_rec_2( 6 );
      println( s"$check" );
      check = fibonacci_rec_2( 7 );
      println( s"$check" );
      check = fibonacci_rec_2( 8 );
      println( s"$check" );
      check = fibonacci_rec_2( 9 );
      println( s"$check" );
    
    }

    /** Tail recursive
     * 
     * A function that computes the nth fibonacci sequence using recursion.
     */
    @tailrec
    def fibonacci_rec_2( n : Int, acc1 : Int = 1, acc2 : Int = 0 ) : Int = {
      
      require( n >= 0 );

      if ( n == 0 ) {

        return 0;

      }

      if ( n <= 1 ) {

        return acc1;

      }

      return fibonacci_rec_2( n - 1, acc1 + acc2, acc1 );

    }

    /**
     * A function that computes the nth fibonacci
     * sequence using a while loop.
     */
    def fibonacci_loop( n : Int ) : Int = {

      if ( n <= 1 ) {
        return 1;
      }
      
      var nth = n;
      var fn = 0; var fn2 = 0; var fn1 = 1;

      while( nth > 1 ) {

        fn = fn2 + fn1;
        fn2 = fn1;
        fn1 = fn;

        nth -= 1;
      }
      
      println( s"result: $fn");
      return fn;
    }

    /**
     * Checks if x is a power of two.
     */
    def isPowerOfTwo( x : Int ) : Boolean = {

      require( x >= 0 );

      if ( x == 0 ) {
        
        return false;

      }
      else if ( x == 1 ) {
        
          return true;

      }
      else if ( x % 2 == 1 ) {
        
        return false;

      }
      else {

        isPowerOfTwo( x / 2 );

      }

    }

    /** Tail recursive
     * 
     * Checks if x is a power of two.
     */
    def recurseToPowerOfTwo( x : Int ) : Int = {

      require( x >= 0 );

      if ( isPowerOfTwo( x ) ) {
        
        return x;

      }
      else {

        return recurseToPowerOfTwo( x + 1 );

      }
    }

    /** Not tail recursive
     * 
     * A function that computes the nth fibonacci
     * sequence using recursion. It is terminating.
     */
    def fibonacci( n : Int ) : Int = {

      require( n >= 0 );

      if ( n <= 1 ) {

        return 1;

      }
      else {

        return fibonacci( n - 1 ) + fibonacci( n - 2 );

      }

    }

    /** Not tail recursive
     * 
     * A function that returns x! using recursion but
     * is not terminating for values x < 0. Add this 
     * precondition with the `require` keyword.
     */
    def factorial( x : Int ) : Int = {

      require( x >= 0);

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

    /** Tail recursive
     * 
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