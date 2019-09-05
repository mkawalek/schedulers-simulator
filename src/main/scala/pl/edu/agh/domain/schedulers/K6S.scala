package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter, Machine, Parameters, SliceId}

class K6S(slicesRequirements: Map[SliceId, Parameters]) extends Scheduler {

  def canScheduleApplicationConsideringSlice(
                                              application: Application,
                                              dataCenter: DataCenter,
                                              sliceRequirements: Map[SliceId, Parameters]
                                            ): Boolean = {

    val alreadyScheduledForSlices =
      dataCenter
        .machines
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

    val paramsAdditional = dataCenter.machines.map(_.parameters).reduce(_ + _) -
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
            .sortBy(_.parameters.dpdkVirtualNICs)
            .reverse
        else
          machines
            .sortBy(_.parameters.dpdkVirtualNICs)

      def thereIsBestPlaceForApplication(application: Application, machine: Machine): Boolean = {
        // SORT ALREADY APPS BY BANDWIDTH USED WITH APP THAT WE ARE TRYING TO PLACE
        val runningApplicationsWhichAreCommunicatingWithThis = application
          .bandWithUsedBetweenApplications
          .filter { case (app, _) =>
            machinesOrdered
              .flatMap(_.runningApplications)
              .map(_.applicationId)
              .contains(app)
          }
          .sortBy { case (_, bandwidth) => bandwidth }

        // CHECK ON WHAT MACHINES WE CAN SCHEDULE THIS APPLICATION
        runningApplicationsWhichAreCommunicatingWithThis.exists {
          case (runningApplicationWhichIsCommunicatingWithThis, _) =>
            machinesOrdered
              .filter(machine => canScheduleApplicationConsideringParamsOnly(application, machine))
              .find(
                _.runningApplications
                  .map(_.applicationId)
                  .contains(runningApplicationWhichIsCommunicatingWithThis)
              )
              .contains(machine)
        }
      }

      // TRY TO FIND PLACE FOR APPLICATION WHERE IT WILL PERFORM BETTER
      def bestEffortAlgorithm: Option[(Machine, Int)] =
        machinesOrdered
          .zipWithIndex
          .collectFirst {
            case (searchingMachine, indexOfSearchingMachine)
              if thereIsBestPlaceForApplication(application, searchingMachine) =>
              searchingMachine.scheduleApplication(application) -> indexOfSearchingMachine
          }

      // IF WE CANT USE ADDITIONAL INFO ABOUT BANDWIDTH - PLACE IT WHERE WE CAN
      def simpleAlgorithm: Option[(Machine, Int)] = machinesOrdered
        .zipWithIndex
        .collectFirst {
          case (searchingMachine, indexOfSearchingMachine)
            if canScheduleApplicationConsideringParamsOnly(application, searchingMachine) &&
              canScheduleApplicationConsideringSlice(
                application,
                DataCenter(machines),
                slicesRequirements) =>

            searchingMachine.scheduleApplication(application) -> indexOfSearchingMachine
        }

      val (machine, index) =
        bestEffortAlgorithm
          .orElse(simpleAlgorithm)
          .getOrElse {
            println(s"NOT ENOUGH RESOURCES, SKIPPING $application")
            machinesOrdered.head -> 0
          }

      machinesOrdered.updated(index, machine)
    })

  override def name: String = "k6s"
}
