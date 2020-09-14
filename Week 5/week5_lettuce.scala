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

    /**
     * Create a curried function:
     * 
     * First call: returns a function 'f' that takes a user defined function
     *             with x * y as the parameter. ex: mult( 5, 10 ) will return
     *             a function that will take a user defined function and pass
     *             (50) to it.
     * Second call: returns the value 'Int' from a user function passed to the
     *              curried function. The result of the first call will be the
     *              parameter passed to the user function.
     * 
     * Again:
     *      First curied () means we pass x and y, and then the result x * y
     *      is the parameter passed to the function 'f' passed to the second 
     *      curried (). The second curried () is a function 'f' which is 
     *      user defined, but must take an int and return an int ( Int => Int ).
     *      The result: pass a function that takes an int, returns an int, the 
     *      function passed gets parameter ( x * y ) from the first call.
     */
    def mult( x : Int, y : Int )( f : Int => Int ) = { f( x * y ) };
    
    def main( args : Array[ String ] ) : Unit = {

        /* Test the curried function */
        val firstCall : Int => Int = curriedFunction( 100 );  // ret: f(y) = 100 + y
        val secondCall : Int = firstCall( 50 );               // ret: f( 50 ) = 100 + 50 = 150
        println( secondCall );

        /* Test another curried function */
        val myFunction : Int => Int = { x => 10 * x };  // define function: Int => Int
        val firstC  = mult( 10, 50 ) {  myFunction }    // x:10, y:50 ret 50 passed to Int => Int
        println( firstC );                              // ret: f( 50 ) = 10 * 50 = 5000

    }

 }
