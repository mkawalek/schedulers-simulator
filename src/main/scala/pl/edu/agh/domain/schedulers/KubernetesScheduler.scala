package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter, Machine}

import scala.util.Random

class KubernetesScheduler extends Scheduler {

  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter =
    DataCenter(jobs.foldLeft(Random.shuffle(dc.machines)) { case (machines, job) =>
      val virtualMachineWithScheduledJob = machines
        .flatMap(_.virtualMachines)
        .collectFirst { case searchingMachine if canScheduleApplicationConsideringParamsOnly(job, searchingMachine) => searchingMachine.scheduleApplication(job) }
        .getOrElse {
          println("NOT ENOUGH RESOURCES SKIPPING JOB")
          machines.flatMap(_.virtualMachines).head
        }

      val newMachines = machines.map { mach =>
        Machine(mach.machineId, mach.virtualMachines.map { vm =>
          if (vm.virtualMachineId == virtualMachineWithScheduledJob.virtualMachineId) virtualMachineWithScheduledJob
          else vm
        })
      }

      newMachines
    })

  override def name: String

  = "kubernetes"
}
