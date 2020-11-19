/**
 *  Week 12 - Object oriented concepts material
 */
class Groceries( val chips: String, val water: String, val apples: String) {

  println("-"*20 + " filling your grocery cart" + "-" * 20)
  private val rice: String = "koshihikari rice. Needs some soy sauce"

  /*- Getters -*/
  def getChips: String = chips
  def getWater: String = water
  def getRice: String = rice

  /*- Setters -*/
  def setChips(newBagOfChips: String): Groceries = {
    new Groceries(newBagOfChips, water, apples)
  }

}
/*-------------------------------------------------------------------------------------------------------------------*/

object Notes {

  def main( args: Array[String] ): Unit = {

    /*- Ex 1: Instantiate a class using the 'class' keyword -*/
    val shopping_1: Groceries = new Groceries(chips="kettle chips", water="calistoga", apples="honeycrisp")
    println( shopping_1.getChips )
    println( shopping_1.getRice )
    println( shopping_1.getWater )



  }

}