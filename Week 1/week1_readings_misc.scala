/**
 * Covering / experimenting with some miscellaneous scala things 
 * that I encountered.
 */

object misc {
  def main( args : Array[ String ] ) : Unit = {

    /* Header */
    println("[ Miscellaneous ]" + "="*50 + "]\n");

    /* Switch statement */
    println( s"switchTest( 1 ): ${ switchTest( 1 ) }" );
    println( s"switchTest( 2 ): ${ switchTest( 2 ) }" );
    println( s"switchTest( 7 ): ${ switchTest( 7 ) }" );

    /**
     * Assign anon functions with parameters and without
     */
    val anon_func_1 = () => { 3 }
    println( anon_func_1() );

    val anon_func_2 = ( x : Int ) => { x + 2 }
    println( anon_func_2( 2 ) );

    val anon_func_3 = ( x : String, y : Int ) => { println( x, y ) }
    anon_func_3( "test", 5 );

    val anon_func_4 = () => { println( "anon_func_4" ) }
    anon_func_4();

    /**
     * Assign anon functions with type declaration and without
     */
    val anon_func_5 : ( String, String ) => String = ( left, right ) => {
        left + ", " + right
    }
    val anon_func_6 = ( left : String, right : String ) => {
        left + ", " + right
    }
    println( anon_func_5( "on_the_left", "on_the_right" ) );
    println( anon_func_6( "on_the_left", "on_the_right" ) );

    /* Triple quotes like in python, in a code block */
    val triple_quotes : String = {
        """	
        Yes, this is a string. It may have newlines
        which I just created. It's nice, perhaps I will
        write an essay. Just kidding, I must learn more
        about this programming language.
        """
    }
    print( triple_quotes );

    /**
     * value :: value puts the right value as the new head of the list.
     */
    println( s"${ List(1, 2, 3) == 1 :: 2 :: 3 :: Nil }" );  // True!

    /**
     * Using reduceLeft ( implemented with tail recursion )
     */
    val test_list : List[ Int ] = List( 1, 2, 3, 4 );		
    val sum_1 : Int = test_list.reduceLeft( _ + _ );
    val sum_2 : Int = test_list.reduceLeft( (a, b) => a + b );
    val smallest : Int = test_list.reduceLeft( _ min _ );
    val largest : Int = test_list.reduceLeft( _ max _ );

    println( s"sum_1: $sum_1\n" +
            s"sum_2: $sum_2 \n" +
        s"smallest: $smallest\n" +
            s"largest: $largest\n" );
        
    /**
     * Using foldLeft
     */
    val sum_start_with_3 : Int = test_list.foldLeft( 3 )( _ + _ );
    val mult_start_with_3 : Int = test_list.foldLeft( 3 )( _ * _ );
    val sum_anon : Int = test_list.foldLeft( 3 )( ( a, b ) => { a + b } );
    println( s"sum_start_with_3: $sum_start_with_3\n" +
            s"mult_start_with_3: $mult_start_with_3\n" +
            s"sum_anon: $sum_anon\n" );

    /* Footer */
    println("\n" + "-"*65 + "\n");
  }
 /* --------------------------------------------------------------- */

  /**
  * Scala switch statement 
  * 
  * Pattern Matching: Check a value against a pattern. 
  */
  def switchTest( x : Int ) : String = {
  	x match {
  		case 1 => "one"
 		case 2 => "two"
 		case _ => "other than one and two"
	}
  }
  
  /**
  * Case class
  */
  abstract class Something {
  	case class test_1( x : String, y : Int ) extends Something;
  	case class test_2( x : String, y : Float ) extends Something;
  	case class test_3( x : String ) extends Something;
  }
}
/* --------------------------------------------------------------- */
