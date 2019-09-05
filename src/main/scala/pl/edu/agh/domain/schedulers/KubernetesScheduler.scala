package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter}

import scala.util.Random

class KubernetesScheduler extends Scheduler {
  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter =
    DataCenter(jobs.foldLeft(Random.shuffle(dc.machines)) { case (machines, job) =>
      val (machine, index) = machines
        .zipWithIndex
        .collectFirst { case (searchingMachine, indexOfSearchingMachine) if canScheduleApplicationConsideringParamsOnly(job, searchingMachine) => searchingMachine.scheduleApplication(job) -> indexOfSearchingMachine }
        .getOrElse {
          println("NOT ENOUGH RESOURCES SKIPPING JOB")
          machines.head -> 0
        }

        machines.updated(index, machine)
    })

  override def name: String = "kubernetes"
}
