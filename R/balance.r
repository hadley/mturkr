#' Retrieves your remaining balance.
#'
#' @param The location of an mturk task, see \code{\link{as.task}} for
#'   specification options
#' @export
get_balance <- function(task = NULL, ...) {
  task <- as.task(task)

  mturk_task_req(task, "GetAccountBalance", ...)
}
