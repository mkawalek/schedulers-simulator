package pl.edu.agh.domain

case class Machine(machineId: MachineId, virtualMachines: List[VirtualMachine])

case object Machine {
  def apply(machineId: MachineId, virtualMachines: VirtualMachine*): Machine = Machine(machineId, List(virtualMachines: _*))
}

case class VirtualMachine(virtualMachineId: VirtualMachineId, parameters: Parameters, runningApplications: List[Application] = List.empty, jobsRunningWithDpdk: List[Application] = List.empty) {
  def parametersLeft: Parameters = if (runningApplications.isEmpty) parameters else parameters - runningApplications.map(_.SLAs).reduce { (jobA, jobB) => jobA + jobB }

  def scheduleApplication(job: Application): VirtualMachine = this.copy(runningApplications = runningApplications :+ job)
}

case class MachineId(value: Long) extends AnyVal

case class VirtualMachineId(value: Long) extends AnyVal

// I ASSUME THAT EVERY MACHINE HAS AT LEAST 1 SIMPLE NIC ADAPTER FOR INTERNET. OTHERS WILL BE USED TO DPDK ONLY
// THIS ASSUMPTION CAN BE EXPANDED FOR FUTURE WORK, E.G. WE HAVE 3 DPDK NICS AND 2 FOR INTERNET - MAYBE IT CAN BE DONE BETTER AND INCREASE PERFORMANCE
case class Parameters(cpu: Double, ram: Double, disk: Double, dpdkNICs: Int = 0, dpdkVirtualNICs: Int = 0, bandwidth: Int = 0) {
  def -(other: Parameters) = Parameters(cpu - other.cpu, ram - other.ram, disk - other.disk, dpdkNICs - other.dpdkNICs, dpdkVirtualNICs - other.dpdkVirtualNICs, bandwidth - other.bandwidth)

  def +(other: Parameters) = Parameters(cpu + other.cpu, ram + other.ram, disk + other.disk, dpdkNICs + other.dpdkNICs, dpdkVirtualNICs + other.dpdkVirtualNICs, bandwidth + other.bandwidth)

  def negateOrZero(value: Double): Double = if (value < 0) -value else 0.toDouble

  def getOnlyNegativeOr0 = Parameters(negateOrZero(cpu), negateOrZero(ram), negateOrZero(disk), negateOrZero(dpdkNICs.toLong).toInt, negateOrZero(dpdkVirtualNICs.toLong).toInt, negateOrZero(bandwidth.toLong).toInt)
}

case object Parameters {
  def predefined(cpu: Double, ram: Double, disk: Double, dpdkNICs: Int, dpdkVirtualNICs: Int, bandwidth: Int): Parameters =
    Parameters(cpu, ram, disk, dpdkNICs, dpdkVirtualNICs = dpdkNICs + dpdkVirtualNICs, if (dpdkNICs > 0) bandwidth * 3 else bandwidth)
}

case class SliceId(value: Int)
