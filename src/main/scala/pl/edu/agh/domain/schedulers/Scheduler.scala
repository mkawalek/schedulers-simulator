package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.DomainObjects.{DC, Job}

trait Scheduler {

  def schedule(dc: DC, jobs: List[Job]): DC

}
