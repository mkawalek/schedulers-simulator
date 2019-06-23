package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.DomainObjects
import pl.edu.agh.domain.DomainObjects.DC

class RoundRobinScheduler extends Scheduler {
  override def schedule(dc: DomainObjects.DC, jobs: List[DomainObjects.Job]): DomainObjects.DC = {
    val roundRobin: Iterator[Int] = Iterator.iterate(0)(_ + 1).map(_ % dc.machines.size)

    val machinesAfterScheduling = jobs.foldLeft(dc.machines) { case (machines, job) =>
      val (machine, indexOfMachine) = machines.zipWithIndex.lift(roundRobin.next).filter(_._1.canScheduleJob(job)).orElse {
        machines.zipWithIndex.collectFirst { case m if m._1.canScheduleJob(job) => m }
      }.getOrElse(throw new IllegalStateException("CANNOT RUN JOB, NOT ENOUGH RESOURCES")) // todo tutaj może lepiej przepuścić algorytm ale niech tego nie scheduluje i zostawi jako "left"

      val finalMachine = machine.copy(runningJobs = machine.runningJobs :+ job)

      machines.updated(indexOfMachine, finalMachine)

    }

    DC(machinesAfterScheduling)
  }
}
