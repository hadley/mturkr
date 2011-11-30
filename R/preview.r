# Should have auto_cleanup = interactive()
# If true, waits for user prompt and then removes
# If false, displays message giving function to use to cleanup 

#' @export
preview_task <- function(task) {
  task <- as.task(task)
  
  if (is.null(task$HitTypeId)) {
    stop("This HIT Type has not been registered yet", call. = FALSE)
  }
  
  id <- task$HitTypeId[1]
  base_url <- "https://workersandbox.mturk.com/mturk/preview?groupId="
  browseURL(str_c(base_url, id))
}
