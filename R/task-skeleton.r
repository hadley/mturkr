#' Create a directory containing the skeleton of a new task.
#'
#' @param task directory where you want to create the task
#' @export
task_skeleton <- function(task) {
  if (file.exists(task)) {
    stop("Directory ", task, " already exists", call. = FALSE)
  }
  
  dir.create(task)
  template <- dir(system.file("task-demo", package = "mturkr"), 
    full.names = TRUE)
  invisible(file.copy(template, task, recursive = TRUE))
}
