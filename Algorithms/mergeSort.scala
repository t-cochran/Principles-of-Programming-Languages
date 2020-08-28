/**
 * Practice learning scala
 * 
 * Merge sort
 */
import scala.collection.mutable.ListBuffer

object mergeSort {

  /**
   * Program entry
   */
  def main( args : Array[ String ] ) : Unit = {

    val unsorted : List[ Int ] = List( 4, 6, 2, 11, 9, 10, 34 );
    val sorted = mergesort( unsorted );
    printList( unsorted );
    printList( sorted );

  }

  /**
   * Mergesort: Merge left and right sub-arrays
   */
  def merge( left : List[ Int ], right : List[ Int ] ) : List[ Int ] = {
    
    ( left, right ) match {
    
      /* Case: left sub-array is empty */
      case( Nil, right ) => {
        return right;
      }

      /* Case: right sub-array is empty */
      case( left, Nil ) => {
        return left;
      }

      /* Case: Merge sub-arrays */
      case( x :: prepend_left, y :: prepend_right ) => {
        if ( x < y ) {
          
          return x :: merge( prepend_left, right );
        
        }   
        else {
        
          return y :: merge( left, prepend_right );
        
        }
      }
    }
  }

  /**
   * Mergesort: Divide left and right sub-arrays
   */
  def mergesort( list : List[ Int ] ) : List[ Int ] = {
    
    /* Divide */
    val mid_idx = list.length >> 1;
    if ( mid_idx == 0 ) {
    
      return list;  // Base Case: sub-list contains a single element
    
    }
    val ( left_arr, right_arr ) = list.splitAt( mid_idx );
    
    /* Merge sub-arrays */
    return merge( mergesort( left_arr ), mergesort( right_arr ) );
  }

  /**
   * Print the contents of a list
   */
  def printList( list : List[ Int ] ) : Unit = {
    list.foreach( ( y ) => print( s"$y " ) );
    println();
  }

}