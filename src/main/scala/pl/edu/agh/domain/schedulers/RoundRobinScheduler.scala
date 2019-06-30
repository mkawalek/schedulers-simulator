package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DC, Job}

class RoundRobinScheduler extends Scheduler {
  override def schedule(dc: DC, jobs: List[Job]): DC = {
    val roundRobin: Iterator[Int] = Iterator.iterate(0)(_ + 1).map(_ % dc.machines.size)

    val machinesAfterScheduling = jobs.foldLeft(dc.machines) { case (machines, job) =>
      val (machine, indexOfMachine) =
        machines
          .zipWithIndex
          .lift(roundRobin.next)
          .filter(_._1.canScheduleJob(job))
          .map { case (foundMachine, indexOfFoundMachine) => foundMachine.scheduleJob(job) -> indexOfFoundMachine }
          .orElse {
            machines
              .zipWithIndex
              .collectFirst { case (searchedMachine, indexOfSearchedMachine) if searchedMachine.canScheduleJob(job) => searchedMachine.scheduleJob(job) -> indexOfSearchedMachine }
          }.getOrElse {
          println(s"SKIPPING JOB $job SINCE THERE IS NOT ENOUGH RESOURCES")
          machines.head -> 0 // TAKE WHATEVER MACHINE BUT DONT APPLY JOB SINCE THERE IS NOT ENOUGH RESOURCES
        }

      machines.updated(indexOfMachine, machine)
    }

    DC(machinesAfterScheduling)
  }
}
