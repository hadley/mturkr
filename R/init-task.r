#' Create a directory containing the skeleton of a new task.
#'
#' @param task directory where you want to create the task
#' @export
init_task <- function(task) {
  if (file.exists(task)) {
    stop("Directory ", task, " already exists", call. = FALSE)
  }
  
  dir.create(task)
  message("Make sure to modify your access key in ", 
    file.path(task, "DESCRIPTION"))
  
  template <- dir(system.file("template", package = "mturkr"), 
    full.names = TRUE)
  invisible(file.copy(template, task, recursive = TRUE))
}
