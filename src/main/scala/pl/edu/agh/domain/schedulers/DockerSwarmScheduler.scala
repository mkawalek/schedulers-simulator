package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DataCenter, Application}
import pl.edu.agh.domain.schedulers.DockerSwarmScheduler.Strategies.{BinPack, Random, Spread}

class DockerSwarmScheduler(options: Map[String, String]) extends Scheduler {

  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter = {
    val strategy = DockerSwarmScheduler.strategyFromString(options(DockerSwarmScheduler.strategyOptionKey))

    DataCenter(jobs.foldLeft(scala.util.Random.shuffle(dc.machines)) { case (machines, job) =>
      val machinesOrderedByStrategy = strategy match {
        case Spread => machines.sortBy(_.runningApplications.size)
        case BinPack => machines.sortWith { case (mA, mB) => mA.parameters.cpu <= mB.parameters.cpu } // TODO I DO NOT KNOW HOW TO COMBINE RAM AND CPU IN ONE CONDITION EFFECTIVELY
        case Random => machines.sortWith((_, _) => scala.util.Random.nextBoolean())
      }

      val (machine, index) = machinesOrderedByStrategy
        .zipWithIndex
        .collectFirst {
          case (searchingMachine, indexOfSearchingMachine) if canScheduleApplicationConsideringParamsOnly(job, searchingMachine) =>
//            println(s"ASSIGNING $job to $searchingMachine")
            searchingMachine.scheduleApplication(job) -> indexOfSearchingMachine
        }
        .getOrElse {
          println(s"NOT ENOUGH RESOURCES SKIPPING JOB $job ")
          machinesOrderedByStrategy.head -> 0
        }

      machinesOrderedByStrategy.updated(index, machine)
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