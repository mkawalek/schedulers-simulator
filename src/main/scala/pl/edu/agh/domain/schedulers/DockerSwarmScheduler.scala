package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{Application, DataCenter, Machine}
import pl.edu.agh.domain.schedulers.DockerSwarmScheduler.Strategies.{BinPack, Random, Spread}

class DockerSwarmScheduler(options: Map[String, String]) extends Scheduler {

  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter = {
    val strategy = DockerSwarmScheduler.strategyFromString(options(DockerSwarmScheduler.strategyOptionKey))

    DataCenter(jobs.foldLeft(dc.machines) { case (machines, job) =>
      val virtualMachinesOrderedByStrategy = strategy match {
        case Spread => machines.flatMap(_.virtualMachines).sortBy(_.runningApplications.size)
        case BinPack => machines.flatMap(_.virtualMachines).sortWith { case (mA, mB) => mA.parametersLeft.cpu <= mB.parametersLeft.cpu } // TODO I DO NOT KNOW HOW TO COMBINE RAM AND CPU IN ONE CONDITION EFFECTIVELY
        case Random => scala.util.Random.shuffle(machines.flatMap(_.virtualMachines))
      }

      val virtualMachineWithScheduledJob = virtualMachinesOrderedByStrategy
        .collectFirst {
          case searchingMachine if canScheduleApplicationConsideringParamsOnly(job, searchingMachine) =>
//            println(s"ASSIGNING $job to $searchingMachine")
            searchingMachine.scheduleApplication(job)
        }
        .getOrElse {
          println(s"NOT ENOUGH RESOURCES SKIPPING JOB $job ")
          virtualMachinesOrderedByStrategy.head
        }


      val newMachines = machines.map { mach =>
        Machine(mach.machineId, mach.virtualMachines.map { vm =>
          if (vm.virtualMachineId == virtualMachineWithScheduledJob.virtualMachineId) virtualMachineWithScheduledJob
          else vm
        })
      }

      newMachines
    })
  }

  override def name: String = s"docker - ${options(DockerSwarmScheduler.strategyOptionKey)}"
}

object DockerSwarmScheduler {
  def strategyFromString(strategy: String): DockerSwarmStrategy = strategy match {
    case "SPREAD" => Spread
    case "BINPACK" => BinPack
    case "RANDOM" => Random
    case other => throw new IllegalArgumentException(s"Unsupported docker swarm strategy $other")
  }

  val strategyOptionKey = "strategy"

  sealed trait DockerSwarmStrategy

  object Strategies {

    case object Spread extends DockerSwarmStrategy

    case object BinPack extends DockerSwarmStrategy

    case object Random extends DockerSwarmStrategy

  }

}