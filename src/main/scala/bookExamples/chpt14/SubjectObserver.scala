package bookExamples.chpt14

abstract class SubjectObserver {
  type S <: Subject
  type O <: Observer

  trait Subject {
    self: S =>                                                       // <1>
    private var observers = List[O]()

    def addObserver(observer: O) = observers ::= observer

    def notifyObservers() = observers.foreach(_.receiveUpdate(self)) // <2>
  }

  trait Observer {
    def receiveUpdate(subject: S)
  }
}
