package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DataCenter, Application}

class RoundRobinScheduler extends Scheduler {
  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter = {
    val roundRobin: Iterator[Int] = Iterator.iterate(0)(_ + 1).map(_ % dc.machines.size)

    val machinesAfterScheduling = jobs.foldLeft(scala.util.Random.shuffle(dc.machines)) { case (machines, job) =>
      val (machine, indexOfMachine) =
        machines
          .zipWithIndex
          .lift(roundRobin.next)
          .filter { case (m, _) => canScheduleApplicationConsideringParamsOnly(job, m) }
          .map { case (foundMachine, indexOfFoundMachine) => foundMachine.scheduleApplication(job) -> indexOfFoundMachine }
          .orElse {
            machines
              .zipWithIndex
              .collectFirst { case (searchedMachine, indexOfSearchedMachine) if canScheduleApplicationConsideringParamsOnly(job, searchedMachine) => searchedMachine.scheduleApplication(job) -> indexOfSearchedMachine }
          }.getOrElse {
          println(s"SKIPPING JOB $job SINCE THERE IS NOT ENOUGH RESOURCES")
          machines.head -> 0 // TAKE WHATEVER MACHINE BUT DONT APPLY JOB SINCE THERE IS NOT ENOUGH RESOURCES
        }

      machines.updated(indexOfMachine, machine)
    }

    DataCenter(machinesAfterScheduling)
  }

  override def name: String = "round robin"
}
