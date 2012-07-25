\name{mturk_task_req}
\alias{mturk_task_req}
\title{Perform operation for given task.}
\usage{
  mturk_task_req(task, operation, host = NULL,
    access_key = NULL, secret_key = NULL, ...)
}
\value{
  an XML object, \code{\link{XMLInternalDocument-class}}
}
\description{
  Perform operation for given task.
}
\section{Keys}{
  To use amazon mturk you need both an access key
  (analogous to a username) and a secret key (analogous to
  a password). Both are available from
  \url{https://aws-portal.amazon.com/gp/aws/developer/account/index.html?action=access-key#access_credentials}.

  There are three ways to specify your keys:

  \itemize{ \item As parameters to any request function:
  \code{access_key}, \code{secret_key} \item As parameters
  in your task description: \code{AccessKey},
  \code{SecretKey} \item As environmental variables:
  \code{AWS_KEY}, \code{AWS_SECRET_KEY} }

  If your code is publically available (such as through a
  public svn or git repository) DO NOT store your secret
  key in your task description, as this will allow anyone
  to charge jobs to your amazon account. If you do
  accidentally publish it, go to the url above and
  deactivate it and generate new access and secret keys.
}

