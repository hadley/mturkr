# Functions for working with tasks

#' Coerce input to a task.
#' 
#' Possibile specifications of task:
#' \itemize{
#'   \item \code{NULL}: use last task
#'   \item path to task
#'   \item task object
#' }
#' @param x object to coerce to a task
#' @export
as.task <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_last_task()
  }
  if (is.null(x)) {
    stop("No task specified", call. = FALSE)
  }
  
  if (is.task(x)) 
    return(x)
  
  pkg <- load_task(x)
  set_last_task(pkg)
}

get_last_task <- NULL
set_last_task <- NULL
local({
  task <- NULL
  
  get_last_task <<- function() {
    task
  }
  set_last_task <<- function(x) {
    task <<- x
    x
  }

})


#' Load task DESCRIPTION into convenient form.
#' @keywords internal
load_task <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")
  
  if (!file.exists(path_desc)) {
    stop("No task description at ", path_desc, call. = FALSE)
  }
  
  desc <- as.list(read.dcf(path_desc)[1, ])
  desc$path <- path
  
  # Parse comma separated components

  # Set defaults
  defaults <- list(
    "Host" = "sandbox"
  )
  desc <- modifyList(defaults, desc)
  
  structure(desc, class = "task")
}

save_task <- function(task) {
  stopifnot(is.task(task))
  
  path <- file.path(task$path, "DESCRIPTION")
  task$path <- NULL
  
  write.dcf(path, x)
}

#' Is the object a task?
#' @keywords internal
#' @export
is.task <- function(x) inherits(x, "task")


