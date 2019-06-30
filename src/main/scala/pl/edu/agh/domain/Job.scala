package pl.edu.agh.domain

trait Job {
  def standardPerformance: Long
  def affinity: MachineParameters
  def calculatedPerformanceOnMachine(machine: Machine): Long = standardPerformance
}

case class StandardJob(standardPerformance: Long, affinity: MachineParameters) extends Job

case class DpdkJob(standardPerformance: Long, affinity: MachineParameters) extends Job {
  require(affinity.dpdk)

  override def calculatedPerformanceOnMachine(machine: Machine): Long = {
    standardPerformance * 2
  }
}
