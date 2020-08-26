/** 
  * File: week1_readings_1.scala 
  * 
  * Working through the scala standard library.
  * 
  * Reference: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html
  */

/* Lists : Immutable */
object ListMethods {

    /**
      *  Main -- Program Entry
      *
      *  Note: 'Unit' is return type if nothing is returned
      *        i.e. a void return.
      */
    def main( args: Array[ String ] ) : Unit = {
        
        /* Header */
        println("[ List Methods ]" + "="*50 + "]\n");

        /* Create a list of integers */
        val x : List[ Int ] = List( 10, 111, 32, 14, 6, 8, 8, 8, 8, 32, 10, 86 );
        printList( x );
        
        /**
          * Check if an element belongs to a list.
          *      List[ A ].exists( p : Int => Boolean ) : List[ Int ]
          */
        println( s"6 exists in list: ${ x.exists( y => { y == 6 } ) }" );
        println( s"2 exists in list: ${ x.exists( y => { y == 2 } ) }" );

        /**
          * Find all elements in a list which are multiples of 3.
          *      List[ A ].filter( p : Int => Boolean ) : List[ Int ]
          */
        println( s"Multiples of 3: ${ x.filter( y => { y % 3 == 0 } ) }");

        /**
          * Reverse the order a list.
          *      List[ A ].reverse
          */
        val x_reversed : List[ Int ] = x.reverse;
        printList( x_reversed );
        
        /**
          * Remove all duplicates from a list.
          *     List[ A ].distinct
          */
        val x_no_dupes : List[ Int ] = x.distinct;
        printList( x_no_dupes );

        /**
          * Sort a list.
          *     List[ A ].sortWith( )
          */
        val x_sorted : List[ Int ] = x.sortWith( _ < _ );
        printList( x_sorted );

        /**
          * Check out the printList_ using the underscore wildcard.
          */
        printList_( x );

        /* Footer */
        println("\n" + "-"*65 + "\n");
    }

    /* --------------------------------------------------------------- */

    /**
      * Method: Print the contents of a list
      */
    def printList( x : List[ Int ] ) : Unit = {
        for ( elt <- x ) {
            print( s"${ elt } ");
        }
        println();
    }

    /**
      * Method: Print contents of a list
      */
    def printList_( x : List[ Int ] ) : Unit = {
        x.foreach( print( _ ) );
        print( "\n" );
        x.foreach( y => print( s"${ y } " ) );
        print( "\n" );
    }
}
/* --------------------------------------------------------------- */

/* Arrays : Mutable */
object ArrayMethods {

    /**
      *  Main -- Program Entry
      *
      *  Note: 'Unit' is return type if nothing is returned
      *        i.e. a void return.
      */
    def main( args: Array[ String ] ) : Unit = {
        
        /* Header */
        println("[ Array Methods ]" + "="*50 + "]\n");

        /**
          * Create an array of integers
          */
        val arr : Array[ Int ] = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
        printArray( arr );

        /**
          * Change some values and print the result
          */
        arr( 0 ) = 100;
        arr( arr.size - 1 ) = 100;
        printArray( arr );

        /**
          * Prepend and append values to the array
          */
        val prepend : Array[ Int ] = 1000 +: arr;
        val append : Array[ Int ] = arr :+ 1000;
        printArray( prepend );
        printArray( append );

        /**
          * Use some library functions
          */
        println( s"Array sum: ${ arr.sum }" );
        println( s"Array max: ${ arr.max }" );

        /**
          * Turn into a list!
          */
        val arr_to_list : List[ Int ]= arr.toList;
        for ( i <- 0.until( arr_to_list.size )) { 
            print( s"${ arr_to_list( i ) } " ); 
        }

        /* Footer */
        println("\n" + "-"*65 + "\n");
    }

    /**
      * Method: Print contents of array using 'for-to-until'
      */
    def printArray( x : Array[ Int ] ) : Unit = {
        for ( i <- 0.until( x.length ) ) {
            print( s"${ x(i) } " );
        }
        println();
    }
}
/* --------------------------------------------------------------- */

/* Maps : Mutable or Immutable */
object MapsPractice {

    /**
      *  Main -- Program Entry
      *
      *  Note: 'Unit' is return type if nothing is returned
      *        i.e. a void return.
      */
    def main( args : Array[ String ] ) : Unit = {
        
        /* Header */
        println("[ Maps ]" + "="*50 + "]\n");

        /* Create a mapping of integers to strings */
        val map1 : Map[ Int, String ] = Map( 1 -> "This", 
                                             2 -> "Is", 
                                             3 -> "a", 
                                             4 -> "map!" );
        
        /* Check if a key exists */
        println( s"Map has key value of 6: ${ map1.contains( 6 ) }" );
        println( s"Map has key value of 2: ${ map1.contains( 2 ) }" );

        /* Add a key : value pair and print the contents of each map */
        val map2 : Map[ Int, String ] = map1 + ( 11 -> "nice" );
        printMap( map1, "map 1" );
        printMap( map2, "map 2" );

        /**
         * Append multiple key : value pairs at once
         */
        val map3 : Map[ Int, String ] = map2 ++ List( ( 12 -> "more" ), 
                                                      ( 17 -> "MORE" ), 
                                                      ( 20 -> "MOOOORE" ) );
        printMap( map3, "map 3" );

        /**
          * Edit the value bound to a key
          */
        val map4 : Map[ Int, String ] = map3 + ( 20 -> "too much!" );
        printMap( map4, "map 4" );

        /**
          * Remove a key : value pair
          */
        val map5 : Map[ Int, String ] = map4 - 17;
        printMap( map5, "map 5" );

        /**
          * Remove multiple key : value pairs at once
          */
        val map6 : Map[ Int, String ] = map5 -- Set( 20, 1, 2, 12, 3, 11 );
        printMap( map6, "map 6" );
        

        /* Footer */
        println("\n" + "-"*65 + "\n");
    }

    /**
     * Print the key : value pairs
     * 
     * Note: ._1 is a method to get the first element of a tuple
     *       ._2 is a method to get the second element of a tuple
     *       ... 
    */
    def printMap( x : Map[ Int, String ], id : String ) : Unit = {
        print( "\n" + id + "\n" );
        x.foreach( y => println( "Key: " + y._1 + ", value: " + y._2 ) );
    }

}
/* --------------------------------------------------------------- */
