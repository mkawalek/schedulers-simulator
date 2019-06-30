package pl.edu.agh.domain.schedulers

import pl.edu.agh.domain.{DC, Job}

trait Scheduler {

  def schedule(dc: DC, jobs: List[Job]): DC

}
