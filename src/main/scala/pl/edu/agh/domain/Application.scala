package pl.edu.agh.domain

trait Application {
  def applicationId: ApplicationId

  def sliceId: SliceId

  val standardPerformance: Double
  val SLAs: Parameters
  val bandwidthUsedBetweenApplications: List[(ApplicationId, Int)]

  def calculatedPerformanceOnMachine(machine: Machine): Double = {
    val virtualMachineRunningThatApplication = machine.virtualMachines.find(_.runningApplications.map(_.applicationId).contains(applicationId)).get
    val applicationsOnSameVirtualMachine = bandwidthUsedBetweenApplications.filter { app => machine.virtualMachines.find(_ == virtualMachineRunningThatApplication).get.runningApplications.map(_.applicationId).contains(app._1) }

    val (applicationsCommunicatingOnSameMachine, applicationsCommunicatingOutside) = bandwidthUsedBetweenApplications.partition { case (app, _) => machine.virtualMachines.exists(vm => vm.runningApplications.map(_.applicationId).contains(app)) }

    val finalApplicationsCommunicatingOnSameMachine = applicationsCommunicatingOnSameMachine.toSet.filterNot(applicationsOnSameVirtualMachine.toSet)

    println(
      finalApplicationsCommunicatingOnSameMachine.size,
      applicationsCommunicatingOnSameMachine.size,
      applicationsCommunicatingOutside.size
    )

    // PERFORMANCE ON OUTSIDE APPLICATIONS
    val allApplicationsOnMachineBandwith = machine.virtualMachines.flatMap(_.runningApplications).filter(x => applicationsCommunicatingOutside.map(_._1).contains(x.applicationId)).map(_.SLAs.bandwidth).sum
    val ratio = allApplicationsOnMachineBandwith.toDouble / machine.virtualMachines.map(_.parameters).reduce(_ + _).bandwidth.toDouble
    val divider = if (ratio < 1.0) 1.0 else ratio

    val outsideBandwidthPerformance = (1.toDouble / divider) * (standardPerformance * applicationsCommunicatingOutside.size)
    val insidePerformance = finalApplicationsCommunicatingOnSameMachine.size * standardPerformance * 1.5 + applicationsOnSameVirtualMachine.size * standardPerformance * 1.8

    outsideBandwidthPerformance + insidePerformance
  }
}

case class ApplicationId(value: Int)

case class StandardApplication(applicationId: ApplicationId, sliceId: SliceId, standardPerformance: Double, SLAs: Parameters, bandwidthUsedBetweenApplications: List[(ApplicationId, Int)] = List.empty) extends Application

case class DpdkApplication(applicationId: ApplicationId, sliceId: SliceId, standardPerformance: Double, SLAs: Parameters, bandwidthUsedBetweenApplications: List[(ApplicationId, Int)] = List.empty) extends Application {
  override def calculatedPerformanceOnMachine(machine: Machine): Double =
    if (machine.virtualMachines.map(_.parameters).reduce(_ + _).dpdkNICs > 0) standardPerformance * 2.5
    else super.calculatedPerformanceOnMachine(machine)
}
