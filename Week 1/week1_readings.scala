/** 
  * Working through the scala standard library.
  * 
  * Reference: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html
  */
object week1 {
    /**
      *  Main -- Program Entry
      *
      *  Note: 'Unit' is return type if nothing is returned
      *        i.e. a void return.
      */
    def main( args: Array[ String ] ) : Unit = {
        
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
    }

    /* Method: Print the contents of a list */
    def printList( x : List[ Int ] ) : Unit = {
        for ( elt <- x ) {
            print( s"${ elt } ");
        }
        println("");
    }
}