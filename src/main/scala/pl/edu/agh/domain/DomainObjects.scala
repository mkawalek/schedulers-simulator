package pl.edu.agh.domain

object DomainObjects {

  case class Job(efficiency: Long, affinity: MachineParameters)

  case class DC(machines: List[Machine])

  case class Machine(parameters: MachineParameters, runningJobs: List[Job] = List.empty) {
    def parametersLeft: MachineParameters = runningJobs.map(_.affinity).reduce { (jobA, jobB) => jobA - jobB}

    def canScheduleJob(job: Job): Boolean = {
      val calculated = this.parameters - job.affinity
      val shouldRunDpdk = job.affinity.dpdk
      val dpdkCondition = if (shouldRunDpdk && !parameters.dpdk) false else true // todo it can be done better but fuck it

      calculated.cpu >= 0 && calculated.ram >= 0 && calculated.disk >= 0 && dpdkCondition
    }
  }

  case class MachineParameters(cpu: Long = 0, ram: Long = 0, disk: Long = 0, dpdk: Boolean = false) {
    def -(other: MachineParameters) = MachineParameters(cpu - other.cpu, ram - other.ram, disk - other.disk, dpdk && other.dpdk)


  }

}
