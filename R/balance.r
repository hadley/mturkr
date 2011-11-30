#' Retrieves your remaining balance.
#'
#' @inheritParams create_task
#' @export
get_balance <- function(task = NULL, ...) {
  task <- as.task(task)

  mturk_task_req(task, "GetAccountBalance", ...)
}
