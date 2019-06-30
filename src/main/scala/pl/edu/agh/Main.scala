package pl.edu.agh

import pl.edu.agh.domain.schedulers._
import pl.edu.agh.domain.{DC, Machine, MachineParameters, StandardJob}

object Main extends App {

  val jobs = List(
    StandardJob(3, MachineParameters(4, 4, 4, dpdk = true)),
    StandardJob(3, MachineParameters(8, 8, 8, dpdk = true)),
    StandardJob(3, MachineParameters(4, 4, 4)),
    StandardJob(3, MachineParameters(4, 4, 4)),
    StandardJob(3, MachineParameters(8, 8, 8)),
    StandardJob(3, MachineParameters(4, 4, 4)),
    StandardJob(3, MachineParameters(8, 8, 8)),
    StandardJob(3, MachineParameters(8, 8, 8))
  )

  val machines = List(
    Machine(MachineParameters(16, 16, 16)),
    Machine(MachineParameters(16, 16, 16)),
    Machine(MachineParameters(16, 16, 16, dpdk = true))
  )

  val dc = DC(machines)

//  val scheduler = new RoundRobinScheduler()
//  val scheduler = new KubernetesScheduler()
//  val scheduler = new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "SPREAD"))
//  val scheduler = new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "BINPACK"))
  val scheduler = new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "RANDOM"))

  println("JOBS")
  println()
  jobs.foreach(println)

  println("MACHINES")
  println()
  machines.foreach(println)

  println()
  println("SCHEDULED")
  val dcAfterScheduling = scheduler.schedule(dc, jobs)

  println(s"CLUSTER UTILIZATION ${dcAfterScheduling.clusterUtilization}")
  println(s"OVERALL JOB PERFORMANCE ${dcAfterScheduling.overallPerformance}")

  dcAfterScheduling.machines.foreach(println)

}
