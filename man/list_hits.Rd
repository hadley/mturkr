\name{list_hits}
\alias{list_hits}
\title{List all HITs associated with this task.}
\usage{
  list_hits(task, ...)
}
\arguments{
  \item{task}{The location of an mturk task, see
  \code{\link{as.task}} for specification options}

  \item{...}{Other parameters passed on to
  \code{\link{mturk_task_req}}.}
}
\description{
  Works by retrieving all HIT associated with user, and
  then filtering to match the HIT type ids listed in the
  DESCRIPTION.
}

