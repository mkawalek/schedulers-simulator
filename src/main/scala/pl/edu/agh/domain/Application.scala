package pl.edu.agh.domain

trait Application {
  def applicationId: ApplicationId
  def sliceId: SliceId
  val standardPerformance: Double
  val SLAs: Parameters
  val bandWithUsedBetweenApplications: List[(ApplicationId, Int)]

  def calculatedPerformanceOnMachine(machine: Machine): Double = {
    val (applicationsCommunicatingOnSameMachine, applicationsCommunicatingOutside) = bandWithUsedBetweenApplications.partition { case (application, _) => machine.runningApplications.map(_.applicationId).contains(application) }
    val allApplicationsCommunication = (applicationsCommunicatingOutside ++ applicationsCommunicatingOnSameMachine).size

    println(applicationsCommunicatingOnSameMachine.size, applicationsCommunicatingOutside.size)

    // PERFORMANCE ON OUTSIDE APPLICATIONS
    val allApplicationsOnMachineBandwith = machine.runningApplications.filter(x => applicationsCommunicatingOutside.map(_._1).contains(x.applicationId)).map(_.SLAs.bandwidth).sum
    val ratio = allApplicationsOnMachineBandwith.toDouble / machine.parameters.bandwidth.toDouble
    val divider = if (ratio < 1.0) 1.0 else ratio

    val outsideBandwithPerformance = (1.toDouble / divider) * (standardPerformance * applicationsCommunicatingOutside.size / allApplicationsCommunication)
    val insidePerformance = standardPerformance * applicationsCommunicatingOnSameMachine.size / allApplicationsCommunication * 2

    outsideBandwithPerformance + insidePerformance
  }
}

case class ApplicationId(value: Int)

case class StandardApplication(applicationId: ApplicationId, sliceId: SliceId, standardPerformance: Double, SLAs: Parameters, bandWithUsedBetweenApplications: List[(ApplicationId, Int)] = List.empty) extends Application

case class DpdkApplication(applicationId: ApplicationId, sliceId: SliceId, standardPerformance: Double, SLAs: Parameters, bandWithUsedBetweenApplications: List[(ApplicationId, Int)] = List.empty) extends Application {
  override def calculatedPerformanceOnMachine(machine: Machine): Double =
    if (machine.parameters.dpdkNICs > 0) standardPerformance * 2.5
    else super.calculatedPerformanceOnMachine(machine)
}
