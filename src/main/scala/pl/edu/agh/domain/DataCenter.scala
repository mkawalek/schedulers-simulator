package pl.edu.agh.domain

case class DataCenter(machines: List[Machine]) {
  require(machines.nonEmpty)

  def clusterUtilization: (Double, Double, Double) = {
    require(machines.flatMap(_.runningApplications).nonEmpty)

    val allResources = machines.map(_.parameters).reduce(_ + _)
    val runningJobResources = machines.flatMap(_.runningApplications).map(_.SLAs).reduce(_ + _)

    // FOR NOW ONLY CPU, RAM & DISK
    (
      runningJobResources.cpu.toDouble / allResources.cpu.toDouble,
      runningJobResources.ram.toDouble / allResources.ram.toDouble,
      runningJobResources.disk.toDouble / allResources.disk.toDouble
    )
  }

  def overallPerformance: Double = machines.flatMap(machine => {
    machine.runningApplications.map(_.calculatedPerformanceOnMachine(machine))
  }).sum
}

