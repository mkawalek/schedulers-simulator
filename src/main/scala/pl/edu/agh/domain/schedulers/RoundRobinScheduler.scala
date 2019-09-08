package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter, Machine}

class RoundRobinScheduler extends Scheduler {

  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter = {
    val roundRobin: Iterator[Int] = Iterator.iterate(0)(_ + 1).map(_ % dc.machines.flatMap(_.virtualMachines).size)

    val machinesAfterScheduling = jobs.foldLeft(dc.machines) { case (machines, job) =>
      val virtualMachineWithScheduledJob =
        machines
          .flatMap(_.virtualMachines)
          .lift(roundRobin.next)
          .filter(m => canScheduleApplicationConsideringParamsOnly(job, m))
          .map(foundMachine => foundMachine.scheduleApplication(job))
          .orElse {
            machines
              .flatMap(_.virtualMachines)
              .collectFirst { case searchedMachine if canScheduleApplicationConsideringParamsOnly(job, searchedMachine) => searchedMachine.scheduleApplication(job) }
          }.getOrElse {
          println(s"SKIPPING JOB $job SINCE THERE IS NOT ENOUGH RESOURCES")
          // TAKE WHATEVER MACHINE BUT DONT APPLY JOB SINCE THERE IS NOT ENOUGH RESOURCES
          machines.flatMap(_.virtualMachines).head
        }


      val newMachines = machines.map { mach =>
        Machine(mach.machineId, mach.virtualMachines.map { vm =>
          if (vm.virtualMachineId == virtualMachineWithScheduledJob.virtualMachineId) virtualMachineWithScheduledJob
          else vm
        })
      }

      newMachines
    }

    DataCenter(machinesAfterScheduling)
  }

  override def name: String = "round robin"
}
