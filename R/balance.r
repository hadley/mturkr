#' Retrieves your remaining balance.
#'
#' @inheritParams register_task
#' @export
get_balance <- function(task = NULL, ...) {
  task <- as.task(task)

  xml <- mturk_task_req(task, "GetAccountBalance", ...)
  xml[["GetAccountBalanceResult"]][["AvailableBalance"]]
}
