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
\value{
  the HIT type ID
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
\section{Required metadata}{
  \itemize{ \item \code{Reward}: Dollars to pay for reward,
  e.g. \code{0.05} for 5 cents.  Can optionally supply
  currency code for non-USD amounts, e.g.  \code{0.05 CAD}.

  \item \code{SingleTimeLimit}: Maximum amount of time a
  worker is allowed to take to complete each HIT. Can be an
  integer (\code{1}), or can specify seconds, minutes,
  hours, days or weeks (or any unique prefix): \code{40
  seconds}, \code{5 minutes}, \code{1 day}, \code{2 w}. }
}

