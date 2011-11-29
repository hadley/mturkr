#' @export
get_balance <- function(task = NULL, ...) {
  task <- as.task(task)

  mturk_task_req(task, "GetAccountBalance", ...)
}
