package pl.edu.agh.domain

case class Job(standardPerformance: Long, affinity: MachineParameters) {
  def calculatedPerformanceOnMachine(machine: Machine): Long = {
    standardPerformance
  }
}
