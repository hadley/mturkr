\name{register_task}
\alias{register_task}
\title{Register a task (HITType).}
\usage{
  register_task(task = NULL, ...)
}
\arguments{
  \item{The}{location of an mturk task, see
  \code{\link{as.task}} for specification options}

  \item{...}{Other parameters passed on to
  \code{\link{mturk_task_req}}.}
}
\description{
  Register your task as a HITType on MTurk. The HIT id is
  stored in your task DESCRIPTION file so that there's
  always a record of which HIT types have been generated
  from this task - there may be multiple if you have
  changed the title, description, keywords etc.
}
\details{
  If this task has been registered previously, and you have
  changed any of the HIT type metadata, you'll get a
  warning notification.
}

