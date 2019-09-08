package pl.edu.agh.domain

case class DataCenter(machines: List[Machine]) {
  require(machines.nonEmpty)

  def clusterUtilization: (Double, Double, Double) = {
    require(machines.flatMap(_.virtualMachines).flatMap(_.runningApplications).nonEmpty)

    val allResources = machines.flatMap(_.virtualMachines).map(_.parameters).reduce(_ + _)
    val runningJobResources = machines.flatMap(_.virtualMachines).flatMap(_.runningApplications).map(_.SLAs).reduce(_ + _)

    // FOR NOW ONLY CPU, RAM & DISK
    (
      runningJobResources.cpu / allResources.cpu,
      runningJobResources.ram / allResources.ram,
      runningJobResources.disk / allResources.disk
    )
  }

  def overallPerformance: Double = machines.flatMap { machine =>
    machine.virtualMachines.flatMap(vm => {
      vm.runningApplications.map(_.calculatedPerformanceOnMachine(machine))
    })
  }.sum / 30
}

