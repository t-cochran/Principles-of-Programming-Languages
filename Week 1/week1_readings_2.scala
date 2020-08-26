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
