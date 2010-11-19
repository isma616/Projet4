package project4

import scala.math.random
import Grid._

class EpidemySimulator extends Simulator {

  protected object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    //val prevalenceRate = 0.01
    val prevalenceRate = 0.05
    val deathRate = 0.25
    val infectionRate = 0.4

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  var persons: List[Person] = List() // to complete: construct list of persons

  def isDangerous(row: Int, col: Int): Boolean = {
    def isDangerous0(persons: List[Person]): Boolean = persons match {
      case x :: xs =>
         if(x.infected && x.row == row && x.col == col)
           true
         else
           isDangerous0(xs)
      case Nil => false
    }
    isDangerous0(persons)
  }

  def isContagious(row: Int, col: Int): Boolean = {
    def isContagious0(persons: List[Person]): Boolean = persons match {
      case x :: xs =>
         if((x.infected || x.sick) && x.row == row && x.col == col)
           true
         else
           isContagious0(xs)
      case Nil => false
    }
    isContagious0(persons)
  }

  (0 until population) foreach(x =>
      persons = new Person(x) :: persons
  )

  (0 until (population * prevalenceRate).toInt) foreach(x =>
      persons(x) infect
  )

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = (random * roomRows).toInt
    var col: Int = (random * roomColumns).toInt

    def scheduleMove {
      // 1. After each move (and also after the beginning of the simulation), a person moves to one
      // of their neighbouring rooms within the next 5 days (with equally distributed probability).
      // Note that the first row is considered to be a neighbour of row eight (and vice versa) ; anal-
      // ogously, the first column is a neighbour of column eight (and vice versa).
      afterDelay(1 + (random * 4).toInt)(move)
    }

    def move {
      if(!dead) {
        val horizontal = (random <= 0.5)
        val newRow = if (horizontal)  ((random * 2).toInt - 1 + row + roomRows   ) % roomRows    else row
        val newCol = if (!horizontal) ((random * 2).toInt - 1 + col + roomColumns) % roomColumns else col
        
        if(!isDangerous(newRow, newCol)) {
          row = newRow
          col = newCol
        }
        if(!dead && !sick && !infected && isContagious(row, col) && random <= infectionRate) {
          infected = true
        }
      }
      scheduleMove
    }

    def sicken {
      //println(id + " is a sick bastard!")
      sick = true
    }
    
    def maybeDie {
      if(random <= deathRate) {
        //println(id + " is dead meat!")
        dead = true
      }
    }

    def immunize {
      if(dead) return
      //println(id + " is one lucky son of a bitch!")
      immune = true
      infected = false
    }

    def reset {
      if(dead) return
      //println(id + " is back in the game!")
      infected = false
      sick = false
      immune = false
    }

    def infect {
      //println(id + " is now infected!")

      // 4. When a person becomes infected, he does not immediately get sick, but enters a phase of
      // incubation in which he is infectious but not sick.
      infected = true
      
      // 5. After 6 days of becoming infected, a person becomes sick and is therefore visibly infectious.
      afterDelay(6)(sicken)

      // 6. After 14 days of becoming infected, a person dies with a probability of 25%. Dead people
      // do not move, but stay visibly infectious.
      afterDelay(14)(maybeDie)

      // 7. After 16 days of becoming infected, a person becomes immune and is no longer visibly
      // infectious, but remains infectious.
      afterDelay(16)(immunize)

      // 8. After 18 days of becoming infected, a person turns healthy. He is now in the same state as
      // he was before his infection, which means that he can get infected again.
      afterDelay(18)(reset)
    }

    scheduleMove
  }

}
