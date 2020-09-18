/** 
 * File: week4_bigStepSemantics.scala 
 * 
 * Working through the material on functors
 */

object functors {

  /* --------------------------------------------------------------- */

  /* Multiply all elements of a list by two and return the new list */
  def multiplyEachEltByTwo( list_1 : List[ Int ], 
                            acc : List[ Int ] = Nil ) : List [ Int ] = {
    ( list_1 ) match {

      /**
       * list_1 has reached its tail (nil), so
       * return acc 
       */
      case Nil => return acc;
      
      /**
       * list_1 has not reached its tail, so look at
       * the next two elements: List( .. head, tail .. )
       */
      case head::tail => {

        /**
         * Create a new list with the current element 'head' multiplied
         * by 2. Then, merge this list to the end of acc.
         */
        val newList = acc ++ List( 2 * head );

        /**
         * Recurse: Pass the next sub-list 'tail' from list_1, and
         * the growing 'newList' as acc.
         */
        return multiplyEachEltByTwo( tail, newList );
      }
    }
  }
  /* --------------------------------------------------------------- */

  /* Remove all even elements from a list, return a new list with odd elements */
  def removeEvenNumbers( list_1 : List[ Int ],
                         acc : List[ Int ] = Nil ) : List[ Int ] = {
    ( list_1 ) match {
      
      /**
       * list_1 has reached its tail (nil), so
       * return acc 
       */
      case Nil => return acc;

      /**
       * list_1 has not reached its tail, so look at
       * the next two elements: List( .. head, tail .. )
       */
      case head::tail => {

        val newList = {
          if ( head % 2 == 0 ) { 
            acc;                // Do not merge even numbers into the new list
          }
          else {
            acc ++ List( head );  // Merge a new list containing the odd number
          }
        }
        return removeEvenNumbers( tail, newList );  // recurse to the next element 'tail'
      }
    }
  }
  /* --------------------------------------------------------------- */

  /* Sum the elements of a list */
  def sumOfList( list_1 : List[ Int ], 
                 sum : Int = 0 ) : Int = {
    ( list_1 ) match {

      case Nil => return sum;

      case head::tail => {
        return sumOfList( tail, sum + head );
      }
    }
  }
  /* --------------------------------------------------------------- */

  /**
   * A functor that applies some function 'f' to each element of a list:
   *
   * Multiply each element by two:
   * functor( List( 1, 2, 3, 4, 5 ),  ( x : Int ) => ( x * 2 ) )
   */
  def functor_1[ A, B ]( list : List[ A ], f : A => B ) : List[ B ] = {

    ( list ) match {

      case Nil => Nil;
      case head::tail => {
        return f( head ) :: functor_1( tail, f );  // apply f to each A
      }
    }
  }
  
  /**
   * A functor that applies some function 'f' to each element of a list:
   *
   * Remove even numbers:
   * println( functor_2( List( 1, 2, 3, 4, 5 ), ( x : Int ) => ( x % 2 != 0 ) ) );
   */
  def functor_2[ A ]( list : List[ A ], f : A => Boolean ) : List[ A ] = {

    ( list ) match {

      case Nil => Nil;
      case head::tail => {
        if ( f( head ) ) {  // Append the element if odd == true
          return head::functor_2( tail, f );
        }
        else {
          return functor_2( tail, f );
        }
      }
    }
  }

  /**
   * A functor that applies some function 'f' to each element of a list:
   *
   * Sum a list of numbers:
   * println( functor_2( List( 1, 2, 3, 4, 5 ), ( x : Int ) => ( x ) ) );
   */
  def functor_3[ A ]( list : List[ A ], f : A => Int ) : Int = {

    ( list ) match {

      case Nil => return 0;
      case head::tail => {  
        return f( head ) + functor_3( tail, f );
      }

    }

  }

  /**
   * A functor that applies some function 'f' to each element of a list:
   *
   * This functor uses two functions applied to each element of the input list.
   * 
   * ex:
   *   val classList : List[ Int ] = List( 4, 8, 4, 8, 7, 7, 7 );
   *     println( s"multiples of 4 divided by 4: ${ 
   *              filterApplyFunctor( classList, 
   *                                  (x : Int) => ( x % 4 == 0 ),
   *                                  (x : Int) => ( x / 4 ) ) }" );  // 1 2 1 2
   * 
   * g: filters elements so only the multiples of 4 are appended
   * f: divide filtered elements by 4
   */
  def functor_4[ A, B ]( list : List[ A ], 
                         g : A => Boolean, 
                         f : A => B ) : List[ B ] = {
    list match {
      case Nil => Nil;
      case i::j if g( i ) => f( i ) :: functor_4( j, g, f );
      case _::j => functor_4( j, g, f );
    }

  }
  /* --------------------------------------------------------------- */


  def main( args : Array[ String ] ) : Unit = {

    /* Experiment with 'fold' operation */
    val list_1 : List[ Int ] = List( 1, 2, 3, 4, 5 );
    val list_2 : List [ String ] = List( "this", "is", "a", "list" );
    /**/ 
    println( s".fold: ${ list_1.fold( 0 )( _ + _ ) }" );                     // 15
    println( s".fold: ${ list_1.fold( 100 )( ( n1, n2 ) => n1 + n2 ) } ");   // 115
    println( s".fold: ${ list_2.fold( "--" )( ( s1, s2 ) => s1 + s2 ) }" );  // --thisisalist
    println( s".foldLeft: ${ list_2.foldLeft( "--" )( ( s1, s2 ) => s1 + s2 ) }" );  // --thisisalist
    println( s".foldRight: ${ list_2.foldRight( "--" )( ( s1, s2 ) => s1 + s2 ) }" );  // thisisalist--
    /* --------------------------------------------------------------- */

    /** 
     * FoldLeft vs. FoldRight:
     * 
     * List(a,b,c).foldRight(z)(f) = f(a, f(b, f(c, z)))
     * List(a,b,c).foldLeft(z)(f) = f(f(f(z, a), b), c)
     */ 
    println( list_1.foldRight( 0 )( ( x, y ) => ( x + y ) ) );
    /**/
    val list_3 = ( 1 to 5 ).toList;
    val list_4 = List( "a", "b", "c", "d", "e" );
    val map_1 = list_3.foldLeft( Map.empty[ Int, String ] )( ( map, v ) => {
        map + ( v -> ( "the letter a: " + list_4.head ) );
    });
    println( map_1 );
    /**/
    val map_2 = list_3.foldLeft( Map.empty[ Int, String ] )( ( map, v ) => {
      map + ( v -> ( "number:" + v.toString ) );
    });
    println( map_2 );
    /**/
    println( list_1.foldLeft( List[ Int ]() )( ( acc : List[ Int ], item : Int ) => item::acc ) );
    /* --------------------------------------------------------------- */

    /**
     * Using functors to perform ops on elements of a list
     */  
    println( multiplyEachEltByTwo( list_1 ) );
    println( functor_1( list_1,  ( x : Int ) => ( x * 2 ) ) );
    /**/
    println( removeEvenNumbers( list_1 ) );
    println( functor_2( list_1, ( x : Int ) => ( x % 2 != 0 ) ) );
    /**/
    println( sumOfList( list_1 ) );
    println( functor_3( list_1, ( x : Int ) => ( x ) ) );
    /* --------------------------------------------------------------- */

  }

}
