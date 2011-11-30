\name{get_assignments}
\alias{get_assignments}
\title{Get all assignments for a task.}
\usage{
  get_assignments(task = NULL, status, ...)
}
\arguments{
  \item{status}{Which status of assignments to retrieved:
  "approved", "rejected", or "submitted" (i.e. assignments
  waiting approval).}

  \item{...}{Other parameters passed on to
  \code{\link{mturk_task_req}}.}
}
\value{
  A data frame with a column for each question (named
  according to the question identifier. Multiple anwsers
  (e.g. from multiple selection questions) are separated
  with the unicode record separater, \code{"\u{001F}"}.
  Additional columns provided by MTurk data are
  assigmnent_id, worker_id, auto_approval_time, accept_time
  and submit_time.
}
\description{
  Most of the time you should not need to call this
  directly - instead use \code{\link{review_task}}.
}

