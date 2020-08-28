/**
 * Practice learning scala
 * 
 * Quicksort ( not tail recursive )
 */
import scala.annotation.tailrec

object quickSort {

  def main( args : Array[ String ] ) : Unit = {

    val test_list : List[ Int ] = List( 3, 11, 45, 9, 2, 19, 8 );
    val sorted = quickSort( test_list );
    printList( sorted );

  }

  /**
   * Quicksort: Merge left and right sub-arrays
   */
  def quickSort( list : List[ Int ] ) : List[ Int ] = {

    if ( list.length <= 1 ) {  // base case
      return list;
    }

    /* Select the middle element as the pivot */
    val pivot = list( list.length >> 1 );

    return quickSort( list.filter( x => pivot >  x ) ) ++
           quickSort( list.filter( x => pivot == x ) ) ++
           quickSort( list.filter( x => pivot <  x ) );
  }

  def printList( list : List[ Int ] ) : Unit = {
    list.foreach( x => print( s"$x ") );
    println();
  }
}