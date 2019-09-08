package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter, Machine, Parameters, SliceId, VirtualMachine}

class K6S(slicesRequirements: Map[SliceId, Parameters]) extends Scheduler {

  // CHECK SLICE CONFIGURATION WHEN SCHEDULING JOBS
  def canScheduleApplicationConsideringSlice(
                                              application: Application,
                                              dataCenter: DataCenter,
                                              sliceRequirements: Map[SliceId, Parameters]
                                            ): Boolean = {

    // ALREADY SCHEDULED RESOURCES PER SLICE
    val alreadyScheduledForSlices =
      dataCenter
        .machines
        .flatMap(_.virtualMachines)
        .flatMap(_.runningApplications)
        .map(app => app.sliceId -> app.SLAs)
        .foldLeft(Map.empty[SliceId, Parameters]) {
          case (acc, (slice, parameters)) =>
            acc.updated(slice, acc.getOrElse(slice, Parameters(0, 0, 0)) + parameters)
        }

    val leftSpaceForSlices =
      sliceRequirements.map {
        case (slice, params) =>
          slice -> (params - alreadyScheduledForSlices.getOrElse(slice, Parameters(0, 0, 0)))
      }

    // UNASSIGNED TO ANY SLICE SPACE
    val paramsAdditional = dataCenter.machines.flatMap(_.virtualMachines).map(_.parameters).reduce(_ + _) -
      sliceRequirements.values.reduce(_ + _)

    val additionalSpace = paramsAdditional -
      leftSpaceForSlices.mapValues(_.getOnlyNegativeOr0).values.reduce(_ + _)

    val leftSpace =
      leftSpaceForSlices.getOrElse(application.sliceId, Parameters(0, 0, 0)) + additionalSpace

    val parametersAfterScheduling = leftSpace - application.SLAs

    val result = parametersAfterScheduling.cpu >= 0 && parametersAfterScheduling.ram >= 0

    result
  }

  override def schedule(dc: DataCenter, applications: List[Application]): DataCenter =
    DataCenter(applications.foldLeft(dc.machines) { case (machines, application) =>

      // SORT MACHINES BY LESS USAGE OF DPDKs
      val machinesOrdered =
        if (application.SLAs.dpdkVirtualNICs > 0)
          machines
            .flatMap(_.virtualMachines)
            .sortBy(_.parameters.dpdkVirtualNICs)
        else
          machines
            .flatMap(_.virtualMachines)
            .sortBy(_.parameters.dpdkVirtualNICs)
            .reverse

      def thereIsBestPlaceForApplication(application: Application): Option[VirtualMachine] = {
        // SORT ALREADY APPS BY BANDWIDTH USED WITH APP THAT WE ARE TRYING TO PLACE
        val runningApplicationsWhichAreCommunicatingWithThis = application
          .bandwidthUsedBetweenApplications
          .filter { case (app, _) =>
            machinesOrdered
              .flatMap(_.runningApplications)
              .map(_.applicationId)
              .contains(app)
          }
          .sortBy { case (_, bandwidth) => bandwidth }

        // CHECK ON WHAT MACHINES THAT HAVE DEPLOYED APPLICATIONS
        // THIS JOB IS HIGHLY COMMUNICATE WITH WE CAN DEPLOY IT
        machinesOrdered
          .filter(machine => canScheduleApplicationConsideringParamsOnly(application, machine))
            .collectFirst {
              case machine if runningApplicationsWhichAreCommunicatingWithThis
                .map(_._1)
                .exists(appId =>machine.runningApplications.map(_.applicationId).contains(appId)) =>
                machine.scheduleApplication(application)
            }
      }

      // TRY TO FIND PLACE FOR APPLICATION WHERE IT WILL PERFORM BETTER
      def simpleAlgorithm = machinesOrdered
        .collectFirst {
          case searchingMachine
            if canScheduleApplicationConsideringParamsOnly(application, searchingMachine)
              &&
              canScheduleApplicationConsideringSlice(
                application,
                DataCenter(machines),
                slicesRequirements)
              =>

            searchingMachine.scheduleApplication(application)
        }

      // MERGE ALL STRATEGIES
      val virtualMachineWithScheduledJob =
        thereIsBestPlaceForApplication(application)
          .orElse(simpleAlgorithm)
          .getOrElse {
            println(s"NOT ENOUGH RESOURCES, SKIPPING $application")
            machinesOrdered.head
          }

      // APPLY NEW CONFIGURATION WITH SCHEDULED APPLICATION
      val newMachines = machines.map { mach =>
        Machine(mach.machineId, mach.virtualMachines.map { vm =>
          if (vm.virtualMachineId == virtualMachineWithScheduledJob.virtualMachineId) {
            virtualMachineWithScheduledJob
          } else {
            vm
          }
        })
      }

      newMachines
    })

  override def name: String = "k6s"
}
