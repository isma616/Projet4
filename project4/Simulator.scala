package project4

class Simulator {
  type Action = () => Unit

  protected type Agenda = List[WorkItem]

  case class WorkItem(time: Int, action: Action)

  protected var agenda: Agenda = List()
  protected var currentTime = 0

  protected def afterDelay(delay: Int)(action: => Unit) {
    val item = WorkItem(currentTime + delay, () => action)
    def insert(ag: Agenda): Agenda =
      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail)
    agenda = insert(agenda)
  }

  protected def next {
    agenda match {
      case List() => {}
      case WorkItem(time, action) :: rest =>
        agenda = rest
        currentTime = time
        action()
    }
  }

  def run {
    println("*** New propagation ***")
    while (!agenda.isEmpty) { next }
  }
}

class Wire {

  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  /*
  // Version similar to andGate:

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  */

   // Version using inverters and stuff

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    val a1bar, a2bar, andresult = new Wire
    inverter(a1, a1bar)
    inverter(a2, a2bar)
    andGate(a1bar, a2bar, andresult)
    inverter(andresult, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demux0(in: Wire, c: List[Wire]): List[Wire] = {
      c match {
        case x :: xs =>
          val xbar, left, right = new Wire
          inverter(x, xbar)
          andGate(xbar, in, left)
          andGate(x,    in, right)
          demux0(left, xs) ::: demux0(right, xs)
        case Nil =>
          List(in)
      }
    }
    connectWires(demux0(in, c), out)
  }

  def connectWires(input: List[Wire], output: List[Wire]) {
    input.zip(output).foreach(pair => connectWire(pair._1, pair._2))
  }

  def connectWire(input: Wire, output: Wire) {
    input addAction(() => output.setSignal(input.getSignal))
  }
  
}

object concreteCircuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(false)
    run

    in2.setSignal(false)
    run
  }


  def demuxExample {
    val in, in1, in2, out1, out2, out3, out4 = new Wire

    probe("in", in)

    probe("in1", in1)
    probe("in2", in2)

    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    probe("out4", out4)

    demux(in, List(in1, in2), List(out1, out2, out3, out4))

    run

    in.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(true)
    run

    in2.setSignal(false)
    run
  }
}

object test extends Application {
  println("===============================")
  println("and example = ")
  concreteCircuit.andGateExample

  println("===============================")
  println("or example = ")
  concreteCircuit.orGateExample

  println("===============================")
  println("demux example = ")
  concreteCircuit.demuxExample
}
