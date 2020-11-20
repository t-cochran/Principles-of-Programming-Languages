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
 *  Class that creates task lists with some encapsulated fields
 *     -> 'private val x' sets private fields as 'invariants' which cannot be directly accessed or modified
 *     -> private var's can be modified using methods
 */
class Tasks {

  private var taskList: List[ String ] = List[ String ]()
  private var numTasks: Int = 0

  def getNumTasks: Int  = numTasks

  def getTask(i: Int): String = {
    assert(i >= 0, s"task index i=$i must be greater than 0")
    if (i > numTasks - 1) {
      throw new IllegalAccessError(s"getTask error: index i=$i out of list range [0, ${numTasks-1}]")
    }
    else {
      taskList(i)
    }
  }

  def addTask(t: String): Unit = {
    taskList = t::taskList
    numTasks = numTasks + 1
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
  private var name = ""

  /*- Give the store a name -*/
  def setName( storeName: String ) : Unit = {
    name = storeName
    println(s"Store name is: $storeName ")
  }

  /*- Fill and return a cart of groceries -*/
  def fillGroceryCart( chips: String, water: String, apples: String ): Groceries = {
    new Groceries( chips, water, apples )
  }

}
/*-------------------------------------------------------------------------------------------------------------------*/

/**
 *  Composition
 *      -> Build bigger structures from smaller ones
 *      -> One object contains a reference to the others
 *
 *  Inheritance
 *      -> Re-use code by implementing functionality common to many objects
 *      -> Documents how concepts are inter-related
 */
class CanvasBox (val xc: Double, val yc: Double, val width: Double, val height: Double) {
  override def toString =
    s"CanvasBox: center at ($xc, $yc) with width= $width, height= $height"
}

/**
 * Base class
 */
abstract class Shape {
  def boundingBox: CanvasBox
  def repOK: Boolean
  def toString: String
}

/**
 * Sub-class: Inherit methods from base class 'Shape'
 */
class Rectangle(val x1: Double, val y1: Double, val x2: Double, val y2: Double) extends Shape {
  def centerX: Double = 0.5 * (x1+ x2)  // define some new methods
  def centerY: Double = 0.5 * (y1 + y2)
  def width: Double = x2 - x1
  def height: Double = y2 - y1

  override def repOK: Boolean = {     // override repOK from the base class
    (x1 < x2) && (y1 < y2)
  }
  override def boundingBox: CanvasBox = {
    new CanvasBox( this.centerX, this.centerY, (x2 - x1), (y2 - y1))
  }
  override def toString: String = {
    s"Rectangle ($x1, $y1) to ($x2, $y2)"
  }
}

/**
 *  Sub-class: Inherit methods from base class 'Rectangle'; implicitly calls the super class constructor
 */
class ColoredRectangle(x1: Double, y1: Double, x2: Double, y2: Double, color: String) extends Rectangle(x1, y1, x2, y2)
class Square(x: Double, y: Double, sideLength: Double) extends Rectangle( x - 0.5 * sideLength,
                                                                          y - 0.5 * sideLength,
                                                                          x + 0.5 * sideLength,
                                                                          y + 0.5 * sideLength ) {
  override def toString: String = {
    s"Square centered at ($x, $y) with side $sideLength"
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
    val store_1 = Store.setName( storeName="safeway" )
    val groceryCart: Groceries = Store.fillGroceryCart(chips="corn chips", water="smart water", apples="granny smith")
    println( groceryCart.getChips )
    println( groceryCart.getRice )
    println( groceryCart.getWater )
    println( groceryCart.getApples )

    /**
     *  Ex 3: Instantiate a task list with some private fields
     */
    val task_list_1: Tasks = new Tasks
    println( task_list_1.getNumTasks )
    task_list_1.addTask("have a task!")
    task_list_1.addTask("here's another one")
    task_list_1.addTask("and one more for good measure")
    println( task_list_1.getNumTasks )
    println( task_list_1.getTask( 2 ) )
  }

}