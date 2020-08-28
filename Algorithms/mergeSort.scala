/**
 * Practice learning scala
 * 
 * Merge sort
 */
import scala.annotation.tailrec

object mergeSort {

  /**
   * Program entry
   */
  def main( args : Array[ String ] ) : Unit = {

    val unsorted : List[ Int ] = List( 4, 6, 2, 11, 9, 10, 34 );
    val sorted = mergesort( unsorted );
    printList( sorted );

  }

  /**
   * Mergesort: Merge left and right sub-arrays
   */
  @tailrec
  def merge( left   : List[ Int ], 
             right  : List[ Int ], 
             sorted : List[ Int ] ) : List[ Int ] = {
    
    ( left, right ) match {
    
      /* Case: left sub-array is empty */
      case( Nil, right ) => {
        return sorted ++ right;
      }

      /* Case: right sub-array is empty */
      case( left, Nil ) => {
        return sorted ++ left;
      }

      /* Case: Merge sub-arrays */
      case( x :: prepend_left, y :: prepend_right ) => {
        if ( x < y ) {
          
          return merge( prepend_left, right, sorted :+ x );
        
        }   
        else {
        
          return merge( left, prepend_right, sorted :+ y );
        
        }
      }
    }
  }

  /**
   * Mergesort: Divide left and right sub-arrays
   */
  def mergesort( list : List[ Int ] ) : List[ Int ] = {
    
    /* Divide */
    var sorted : List[ Int ] = Nil;
    val mid_idx = list.length >> 1;
    if ( mid_idx == 0 ) {
    
      return list;  // Base Case: sub-list contains a single element
    
    }
    val ( left_arr, right_arr ) = list.splitAt( mid_idx );
    
    /* Merge sub-arrays */
    return merge( mergesort( left_arr ), mergesort( right_arr ), sorted );
  }

  /**
   * Print the contents of a list
   */
  def printList( list : List[ Int ] ) : Unit = {
    list.foreach( ( y ) => print( s"$y " ) );
    println();
  }

}