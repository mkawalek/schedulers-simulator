package pl.edu.agh.domain

case class Machine(parameters: MachineParameters, runningJobs: List[Job] = List.empty) {
  def parametersLeft: MachineParameters = runningJobs.map(_.affinity).reduce { (jobA, jobB) => jobA - jobB }

  def canScheduleJob(job: Job): Boolean = {
    val calculated = this.parameters - runningJobs.map(_.affinity).reduceLeftOption(_ + _).getOrElse(MachineParameters()) - job.affinity
    val shouldRunDpdk = job.affinity.dpdk
    val dpdkCondition = if (shouldRunDpdk && !parameters.dpdk) false else true // todo it can be done better but fuck it

    calculated.cpu >= 0 && calculated.ram >= 0 && calculated.disk >= 0 && dpdkCondition
  }

  def scheduleJob(job: Job): Machine = this.copy(runningJobs = runningJobs :+ job)
}

case class MachineParameters(cpu: Long = 0, ram: Long = 0, disk: Long = 0, dpdk: Boolean = false) {
  def -(other: MachineParameters) = MachineParameters(cpu - other.cpu, ram - other.ram, disk - other.disk, dpdk && other.dpdk)

  def +(other: MachineParameters) = MachineParameters(cpu + other.cpu, ram + other.ram, disk + other.disk, dpdk && other.dpdk)

}