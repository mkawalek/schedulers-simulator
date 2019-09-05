package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DataCenter, Application}

class MesosScheduler extends Scheduler {
  override def schedule(dc: DataCenter, jobs: List[Application]): DataCenter = dc

  override def name: String = "mesos"
}
