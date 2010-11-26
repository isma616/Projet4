package project4

import scala.math.random
import Grid._

class EpidemySimulator extends Simulator {

  // Determine the average number of dead people after 150 days over a set of 5 experiment runs.
  // 1st run: 69
  // 2nd run: 62
  // 3rd run: 76
  // 4th run: 0
  // 5th run: 69
  // Mean = 55.2 deaths

  // With air traffic:
  // 1st run: 77
  // 2nd run: 77
  // 3rd run: 89
  // 4th run: 84
  // 5th run: 82
  // Mean = 81.8 deaths

  // Reduce mobility act, with air traffic:
  // 1st run: 62
  // 2nd run: 71
  // 3rd run: 20
  // 4th run: 53
  // 5th run: 55
  // Mean = 52.2 deaths

  // Chosen few act, with air traffic:
  // 1st run: 72
  // 2nd run: 0
  // 3rd run: 82
  // 4th run: 74
  // 5th run: 74
  // Mean = 60.4 deaths

  protected object SimConfig {
    val population: Int = 300
    val roomRows   : Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val deathRate = 0.25
    val infectionRate = 0.4

    val airplanes = false
    val airplaneProbability = 0.01

    val chosenFew = false
    val vipRate = 0.05
    
    val reduceMobility = false
  }

  import SimConfig._

  var persons: List[Person] = List()

  def forPersons(row: Int, col: Int, action: (Person) => Boolean) = {
    def forPersons0(persons: List[Person]): Boolean = persons match {
      case x :: xs =>
        if(x.row == row && x.col == col && action(x)) {
          true
        } else forPersons0(xs)
      case Nil => {
        false
      }
    }
    forPersons0(persons)
  }

  def isDangerous(row: Int, col: Int): Boolean = {
    forPersons(row, col, x => x.infected)
  }

  def isContagious(row: Int, col: Int): Boolean = {
    forPersons(row, col, x => x.infected || x.sick || x.dead)
  }

  def contaminate(row: Int, col: Int, exception: Int) {
    forPersons(row, col, x => {
      if(x.id != exception && random <= infectionRate) x.infect
      false
    })
  }

  class Person (val id: Int) {
    var infected = (random <= prevalenceRate)
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = (random * roomRows).toInt
    var col: Int = (random * roomColumns).toInt

    var nextRow: Int = row
    var nextCol: Int = col

    def scheduleMove {
      // 1. After each move (and also after the beginning of the simulation), a person moves to one
      // of their neighbouring rooms within the next 5 days (with equally distributed probability).
      // Note that the first row is considered to be a neighbour of row eight (and vice versa) ; anal-
      // ogously, the first column is a neighbour of column eight (and vice versa).
      val proba = 1 + (random * 4).toInt
      afterDelay(if(reduceMobility) (if(sick) proba * 4 else proba * 2) else proba)(move)
    }

    def move {
      if(!dead) {
        def testMove(newRow: Int, newCol: Int): Boolean = {
          if(!isDangerous(newRow, newCol)) {
            nextRow = newRow
            nextCol = newCol
            // apply decisions at the end
            afterDelay(0)(applyDecisions)
            return true
          }
          false
        }

        val moves = scala.util.Random.shuffle(List(
          ((row - 1 + roomRows) % roomRows, col),
          ((row + 1)            % roomRows, col),
          (row, (col + 1)               % roomColumns),
          (row, (col - 1 + roomColumns) % roomColumns)
        ))

        def testMoves(): Unit = for(m <- moves) {
            if(testMove(m._1, m._2)) return
        }
        testMoves()
        
        if(airplanes && random <= airplaneProbability) {
          nextRow = (random * (roomRows   )).asInstanceOf[Int]
          nextCol = (random * (roomColumns)).asInstanceOf[Int]
          afterDelay(0)(applyDecisions)
        }
      }
      scheduleMove
    }

    def applyDecisions {
      // move
      row = nextRow
      col = nextCol

      // if there are contagious people (infected sick or dead) we might get infected ourselves!
      if(isContagious(row, col) && random <= infectionRate)
        infect
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
      if(immune || dead || infected) return

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

    if(chosenFew && random <= vipRate) immune = true
    if(random <= prevalenceRate) infect

  }

}
