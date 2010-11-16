package project4

import scala.math.random
import Grid._

class EpidemySimulator extends Simulator {

  protected object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  var persons: List[Person] = List() // to complete: construct list of persons

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = (random * roomRows).toInt
    var col: Int = (random * roomColumns).toInt

    //
    // to complete with simulation logic
    //
  }

}
