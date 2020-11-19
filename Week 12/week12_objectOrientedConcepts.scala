/**
 *  Week 12 - Object oriented concepts material
 */

/**
 *  Classes in scala
 *      -> Can create many instances of objects
 *      -> Can have private values, getter/setter methods
 */
class Groceries( val chips: String, val water: String, val apples: String ) {

  println("-"*20 + " filling your grocery cart" + "-" * 20)
  private val rice: String = "koshihikari rice. Needs some soy sauce"

  /*- Getters -*/
  def getChips: String = chips
  def getWater: String = water
  def getRice: String = rice
  def getApples: String = apples

  /*- Setters -*/
  def setChips(newBagOfChips: String): Groceries = {
    new Groceries(newBagOfChips, water, apples)
  }

}
/*-------------------------------------------------------------------------------------------------------------------*/

/**
 *  Objects in scala
 *      -> A class that has exactly one instance
 *      -> Created lazily whenever it is referenced; already created; we cannot create more instances of it
 *      -> [object x extends z] will create an object 'x' an instance of trait 'z' and can be passed anywhere as a 'z'
 */
object Store {

  /*- Enter the store -*/
  println( "-" * 20 + "You have accessed the wonderful methods of STORE!" + "-" * 20 )

  /*- Give the store a name -*/
  def name( storeName: String ) : Unit = println( s"Store name is: $storeName ")

  /*- Fill and return a cart of groceries -*/
  def fillGroceryCart( chips: String, water: String, apples: String ): Groceries = {
    new Groceries( chips, water, apples )
  }

}
/*-------------------------------------------------------------------------------------------------------------------*/



object Notes {

  def main( args: Array[String] ): Unit = {

    /*- Ex 1: Instantiate a class object -*/
    val shopping_1: Groceries = new Groceries(chips="kettle chips", water="calistoga", apples="honeycrisp")
    println( shopping_1.getChips )
    println( shopping_1.getRice )
    println( shopping_1.getWater )
    println( shopping_1.getApples )

    /*- Ex 2: Access object methods -*/
    val store_1 = Store.name( storeName="safeway" )
    val groceryCart: Groceries = Store.fillGroceryCart(chips="corn chips", water="smart water", apples="granny smith")
    println( groceryCart.getChips )
    println( groceryCart.getRice )
    println( groceryCart.getWater )
    println( groceryCart.getApples )
  }

}