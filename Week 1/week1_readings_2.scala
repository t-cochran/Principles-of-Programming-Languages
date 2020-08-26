/** 
  * File: week1_readings_2.scala 
  * 
  * Working through the scala standard library.
  * 
  * Reference: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html
  */

/* Sets : Iterable, no duplicate elements */
object SetsPractice {
  /**
    *  Main -- Program Entry
    *
    *  Note: 'Unit' is return type if nothing is returned
    *        i.e. a void return.
    */
  def main( args : Array[ String ] ) : Unit = {

    /* Header */
    println("[ Sets ]" + "="*50 + "]\n");

    /* Create a set */
    val books = Set( "Don Quixote", 
                      "Crime and Punishment", 
                      "Anna Karenina", 
                      "Moby Dick" );
    printSet( books );

    /* Add to the set */
    val more_books = books + "A Critique of Pure Reason";      
    printSet( more_books );

    /* Add several books to the set */
    val more_more_books = more_books ++ Set( "War and Peace",
                                              "La Peste",
                                              "Middlemarch" );
    printSet( more_more_books );

    /* Remove several books from the set */
    val less_books = more_more_books -- Set( "La Peste", 
                                              "Crime and Punishment" );
    printSet( less_books );

    /* Intersection two ways */
    val intersect_1 = more_more_books.intersect( less_books );
    val intersect_2 = more_more_books & less_books;
    printSet( intersect_1 );
    
    /* Verify intersects */
    val check_1 : Boolean = intersect_1 == intersect_2;
    println( s"${ check_1 }" );

    /* Difference two ways */
    val diff_1 = more_more_books.diff( books );
    val diff_2 = more_more_books &~ books;
    printSet( diff_1 );

    /* Verify difference */
    val check_2 : Boolean = diff_1 == diff_2;
    println( s"${ check_2 }" );

    /* Footer */
    println("\n" + "-"*65 + "\n");
  }

  /**
   * Print contents of the set.
   */
  def printSet( x : Set[ String ] ) : Unit = {
    x.foreach( y => println( "Item: " + y ) );
    println();
  }
}
/* --------------------------------------------------------------- */

/* Functions : first class entities */
object functionPractice {
  /**
    *  Main -- Program Entry
    *
    *  Note: 'Unit' is return type if nothing is returned
    *        i.e. a void return.
    */
  def main( args : Array[ String ] ) : Unit = {
    
    /* Header */
    println("[ Functions ]" + "="*50 + "]\n");
    
    /* Returns (x + 2)^2 */
    val ret_1 = function_1( 3 );
    println( s"ret_1: ${ ret_1 } \n" );

    /* Returns string repeated n times */
    val ret_2 = function_2( "-TEST-", 5 );
    println( s"ret_2: ${ ret_2 } \n" );

    /* Returns string repeated n times */
    val ret_3 = function_3( "-TEST2-", 5 );
    println( s"ret_3: ${ ret_3 } \n" );

    /* Returns x! */
    val ret_4 = factorial( 4 );
    println( s"4! = ${ ret_4 }" );
    val ret_5 = factorial( 5 );
    println( s"5! = ${ ret_5 }" );

    /* Function that calls another function */
    val ret_6 = function_4( factorial, 5 );
    println( s"function calling factorial( 5 ): ${ ret_6 }" );

    /* Footer */
    println("\n" + "-"*65 + "\n");
  }

  /**
   * Pass x, return (x+2)^2
   */
  def function_1( x : Int ) : Int = {
    val y = x + 2;
    val z = y * y;
    return z;
  }

  /**
   * Pass string and integer, return string repeated integer times
   */
  def function_2( x : String, y : Int ) : String = {
    var ret = "";
    for ( i <- 1 to y ) {
      ret += x;
    }
    return ret;
  }

  /**
   * Pass string and integer, return string repeated integer times
   */
  def function_3( x : String, y : Int) : String = {
    ( 1 to y ).foldLeft( "" ) ( ( v, _ ) => v + x )
  }

  /**
   * Computes the factorial x! recursively
   */
  def factorial( x : Int ) : Int = {
    if ( x <= 0 ) {
      return 1;
    }
    else {
      return x * factorial( x - 1 );
    }
  }

  /**
   * Function as arguments to other functions
   * 
   * Note -- func : Int => Int is a function that takes an integer
   *         and returns an integer
   */
  def function_4( func : Int => Int, x : Int) : Int = {
    return func( x );
  }
}
/* --------------------------------------------------------------- */