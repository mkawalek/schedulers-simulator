package pl.edu.agh

import java.nio.file.{Files, Paths}

import pl.edu.agh.domain.schedulers._
import pl.edu.agh.domain.{ApplicationId, DataCenter, DpdkApplication, Machine, MachineId, Parameters, SliceId, StandardApplication, VirtualMachine, VirtualMachineId}

import scala.util.Random

object Main extends App {

  val sliceRequirements = Map[SliceId, Parameters](
    SliceId(1) -> Parameters(20, 20, 0, 20, 20, 1000),
    SliceId(2) -> Parameters(20, 20, 0, 20, 20, 1000),
    SliceId(3) -> Parameters(20, 20, 0, 20, 20, 1000),
    SliceId(4) -> Parameters(20, 20, 0, 20, 20, 5000),
  )

  // SPGW - GATEWAY
  // MME - JAVKA
  // HSS - BAZKA

  val spgwApplications = (1 to 50).map(idx =>
    DpdkApplication(
      ApplicationId(idx),
      SliceId(idx % 4 + 1),
      standardPerformance = 20.0,
      SLAs = Parameters.predefined(1, 1, 0, dpdkNICs = 1, dpdkVirtualNICs = 0, bandwidth = 70),
      bandwidthUsedBetweenApplications =
        // WIDTH MME
        (51 to 100).filter(_ => Random.nextBoolean()).map(mme =>
          ApplicationId(mme) -> Random.nextInt(70)
        ).toList
    )
  )

  // MME
  val mmeApplications = (51 to 100).map(idx =>
    StandardApplication(
      ApplicationId(idx),
      SliceId(idx % 4 + 1),
      standardPerformance = 20.0,
      SLAs = Parameters.predefined(1, 1, 0, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 50),
      bandwidthUsedBetweenApplications =
        // WITH SPGW
        (1 to 50).filter(_ => Random.nextBoolean()).map(spgw =>
          ApplicationId(spgw) -> Random.nextInt(70)
        ).toList ++
          // WIDTH HSS
          (101 to 150).filter(_ => Random.nextBoolean()).map(hss =>
            ApplicationId(hss) -> Random.nextInt(40)
          ).toList
    )
  )

  // HSS
  val hssApplications = (101 to 150).map(idx =>
    StandardApplication(
      ApplicationId(idx),
      SliceId(idx % 4 + 1),
      standardPerformance = 20.0,
      SLAs = Parameters.predefined(1, 1, 0, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 40),
      bandwidthUsedBetweenApplications =
        // WIDTH MME
        (51 to 100).filter(_ => Random.nextBoolean()).map(hss =>
          ApplicationId(hss) -> Random.nextInt(40)
        ).toList ++
          (101 to 150).filter(_ => Random.nextBoolean()).map(hss =>
            ApplicationId(hss) -> Random.nextInt(40)
          ).toList
    )
  )

  val jobs = (hssApplications ++ spgwApplications ++ mmeApplications).toList

  var results: List[String] = List.empty
  var resultsSlice: List[String] = List.empty

  for (scheduler <- List(
    new K6S(sliceRequirements),
    new KubernetesScheduler(),
    new RoundRobinScheduler(),
    new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "SPREAD")),
    new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "BINPACK")),
    new DockerSwarmScheduler(Map(DockerSwarmScheduler.strategyOptionKey -> "RANDOM"))
  )) {

    val machines = List(
      Machine(
        MachineId(1),
        VirtualMachine(VirtualMachineId(1), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(2), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(3), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000))
      ),

      Machine(
        MachineId(2),
        VirtualMachine(VirtualMachineId(4), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(5), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(6), Parameters.predefined(16, 16, 30, dpdkNICs = 0, dpdkVirtualNICs = 0, bandwidth = 1000))
      ),
      Machine(
        MachineId(3),
        VirtualMachine(VirtualMachineId(7), Parameters.predefined(32, 32, 30, dpdkNICs = 64, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(8), Parameters.predefined(32, 32, 30, dpdkNICs = 64, dpdkVirtualNICs = 0, bandwidth = 1000)),
        VirtualMachine(VirtualMachineId(9), Parameters.predefined(32, 32, 30, dpdkNICs = 64, dpdkVirtualNICs = 0, bandwidth = 1000))
      )
    )

    val dc = DataCenter(machines)

    val dcAfterScheduling = scheduler.schedule(dc, jobs)

    val sliceResult = dcAfterScheduling.machines.flatMap(_.virtualMachines).flatMap(_.runningApplications).foldLeft(Map.empty[SliceId, Parameters]) {
      case (acc, app) => acc.updated(app.sliceId, acc.getOrElse(app.sliceId, Parameters(0, 0, 0)) + app.SLAs)
    }
    println("DONE")

    resultsSlice = resultsSlice :+ s"${scheduler.name},${sliceResult(SliceId(1)).cpu},${sliceResult(SliceId(2)).cpu},${sliceResult(SliceId(3)).cpu},${sliceResult.getOrElse(SliceId(4), Parameters(0, 0, 0)).cpu}"

    results = results :+ s"${scheduler.name},${dcAfterScheduling.clusterUtilization.productIterator.mkString(",")},${dcAfterScheduling.overallPerformance}"
  }

  val columns = "scheduler,cpu-utilization,mem-utilization,disk-utilization,cluster-performance"
  val sliceColumns = "scheduler,slice 1,slice 2,slice 3,slice 4"

  Files.write(Paths.get("./results.csv"), (List(columns) ++ results).mkString("\n").getBytes())
  Files.write(Paths.get("./slices.csv"), (List(sliceColumns) ++ resultsSlice).mkString("\n").getBytes())

  results.foreach(println)

  println("SLICES")

  resultsSlice.foreach(println)

}
