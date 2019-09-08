package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DataCenter, Application, VirtualMachine, Parameters}

trait Scheduler {

  def name: String

  def schedule(dc: DataCenter, jobs: List[Application]): DataCenter

  def canScheduleApplicationConsideringParamsOnly(application: Application, machine: VirtualMachine): Boolean = {
    val calculated = machine.parameters - machine.runningApplications.map(_.SLAs).reduceLeftOption(_ + _).getOrElse(Parameters(0, 0, 0, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 0)) - application.SLAs

    calculated.cpu >= 0 && calculated.ram >= 0 && calculated.disk >= 0
  }

}
