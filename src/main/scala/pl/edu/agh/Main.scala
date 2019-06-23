package pl.edu.agh

import pl.edu.agh.domain.DomainObjects.{DC, Job, Machine, MachineParameters}
import pl.edu.agh.domain.schedulers.RoundRobinScheduler

object Main extends App {

  val jobs = List(
    Job(3, MachineParameters(1, 1, 1)),
    Job(3, MachineParameters(1, 1, 3)),
    Job(3, MachineParameters(1, 2, 1)),
    Job(3, MachineParameters(4, 1, 1)),
    Job(3, MachineParameters(1, 2, 1)),
    Job(3, MachineParameters(1, 1, 3)),
    Job(3, MachineParameters(2, 2, 1))
  )


  val machines = List(
    Machine(MachineParameters(16, 16, 16)),
    Machine(MachineParameters(16, 16, 16)),
    Machine(MachineParameters(16, 16, 16))
  )

  val dc = DC(machines)

  val scheduler = new RoundRobinScheduler()


  println("JOBS")
  jobs.foreach(println)

  println("MACHINES")
  machines.foreach(println)

  println("SCHEDULED")
  scheduler.schedule(dc, jobs).machines.foreach(println)

}
