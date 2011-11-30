\name{review_task}
\alias{review_task}
\title{Review task.}
\usage{
  review_task(task = NULL, ..., quiet = FALSE)
}
\arguments{
  \item{...}{Other parameters passed on to
  \code{\link{mturk_task_req}}.}
}
\description{
  Reviewing takes place in two steps: reviewable HITs are
  downloaded and stored in \code{hit-review.csv}, and then
  the review column is inspected and matched to approve or
  reject (or prefixes) and those reviews are uploaded back
  to MTurk and removed from the csv file.
}

