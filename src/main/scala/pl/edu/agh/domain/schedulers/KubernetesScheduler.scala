package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DC, Job}

class KubernetesScheduler extends Scheduler {
  override def schedule(dc: DC, jobs: List[Job]): DC =
    DC(jobs.foldLeft(dc.machines) { case (machines, job) =>
      val (machine, index) = machines
        .zipWithIndex
        .collectFirst { case (searchingMachine, indexOfSearchingMachine) if searchingMachine.canScheduleJob(job) => searchingMachine.scheduleJob(job) -> indexOfSearchingMachine }
        .getOrElse {
          println("NOT ENOUGH RESOURCES SKIPPING JOB")
          machines.head -> 0
        }

        machines.updated(index, machine)
    })
}
