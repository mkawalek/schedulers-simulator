package pl.edu.agh.domain

case class DC(machines: List[Machine]) {
  require(machines.nonEmpty)

  def clusterUtilization: (Double, Double, Double) = {
    require(machines.flatMap(_.runningJobs).nonEmpty)

    val allResources = machines.map(_.parameters).reduce(_ + _)
    val runningJobResources = machines.flatMap(_.runningJobs).map(_.affinity).reduce(_ + _)

    (
      runningJobResources.cpu.toDouble / allResources.cpu.toDouble,
      runningJobResources.ram.toDouble / allResources.ram.toDouble,
      runningJobResources.disk.toDouble / allResources.disk.toDouble
    )
  }

  def overallPerformance: Long = machines.flatMap(machine => {
    machine.runningJobs.map(_.calculatedPerformanceOnMachine(machine))
  }).sum
}

