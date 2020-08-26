object week1 {
    /**
     *  Main -- Program Entry
     *
     *  Note: 'Unit' is return type if nothing is returned
     *        i.e. a void return.
     */
    def main( args: Array[ String ] ) : Unit = {
        
        /* Create a list of integers */
        val x : List[ Int ] = List( 10, 111, 32, 14, 6, 8, 86 );
        
        /**
         * Check if an element belongs to a list.
         *      exists(p : Int => Boolean) : List[ Int ]
         */
        println( s"6 exists in list: ${ x.exists( y => { y == 6 } ) }" );
        println( s"2 exists in list: ${ x.exists( y => { y == 2 } ) }" );

        /**
         * Find all elements in a list which are multiples of 3.
         *      filter(p : Int => Boolean) : List[ Int ]
         */
        println( s"Multiples of 3: ${ x.filter( y => { y % 3 == 0 } ) }");

        /**
         * Reverse a list.
         *      
         */

    }

    /* Method: Print the contents of a list */
    def printList( x : List[ Int ] ) : Unit = {

        for ( elt <- x ) {
            
            println( s"${ elt }");

        }
    }
}