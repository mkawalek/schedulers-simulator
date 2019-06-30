package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DC, Job}

class MesosScheduler extends Scheduler {
  override def schedule(dc: DC, jobs: List[Job]): DC = {
    // TODO IN PROGRESS BECAUSE IT HAS DIFFERENT ARCHITECTURE AND PROBABLY IT WON'T BE ANY BETTER THEN K8S AND SWARM IN TERMS OF DEPLOYING 5G OVER THERE
    dc
  }
}
