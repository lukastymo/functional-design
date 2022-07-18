package net.degoes

object Hello extends App {
  class Scheduler {
    def bar = "bar"
    def foo: Scheduler = new Scheduler {
      println(bar)
    }
  }

  new Scheduler().foo
}
