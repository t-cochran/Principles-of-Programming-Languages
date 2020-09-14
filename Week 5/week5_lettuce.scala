/** 
 * File: week5_lettuce.scala
 * 
 * Working through the material on the Let language
 */

 object lettuceScratch {

    /**
     * Create a curried function 
     * 
     * First call: returns a function with 'x' passed into the definition
     * Second call: calls the returned function with 'y' passed
     */
    def curriedFunction( x : Int )( y : Int ) = x + y;

    def main( args : Array[ String ] ) : Unit = {

        /* Test the curried function */
        val firstCall : Int => Int = curriedFunction( 100 );  // ret: f(y) = 100 + y
        val secondCall : Int = firstCall( 50 );               // ret: f( 50 ) = 100 + 50 = 150
        println( secondCall );



    }

 }
