\name{unpublish_task}
\alias{unpublish_task}
\title{Unpublish all published HITs.}
\usage{
  unpublish_task(task, ..., quiet = FALSE)
}
\arguments{
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
}
