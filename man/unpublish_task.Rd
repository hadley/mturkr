\name{unpublish_task}
\alias{unpublish_task}
\title{Unpublish all published HITs.}
\usage{
  unpublish_task(task, ..., quiet = FALSE)
}
\arguments{
  \item{task}{The location of an mturk task, see
  \code{\link{as.task}} for specification options}

  \item{...}{Other parameters passed on to
  \code{\link{mturk_task_req}}.}
}
\value{
  Invisibly returns disabled HIT ids
}
\description{
  This operation "removes a HIT from the Amazon Mechanical
  Turk marketplace, approves all submitted assignments that
  have not already been approved or rejected, and disposes
  of the HIT and all assignment data.".
}
\details{
  In MTurk API terminology this is disabling the HIT.

  This function removes both HITs listed in the template
  ("linked" HITs) and other HITs associated with this task
  that are on MTurk, but not recorded in the csv file
  ("unlinked" HITs).
}

