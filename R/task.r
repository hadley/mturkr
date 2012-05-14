#' Coerce input to a task.
#' 
#' Possibile specifications of task:
#' \itemize{
#'   \item \code{NULL}: use last task
#'   \item path to task
#'   \item task object
#' }
#' @param x object to coerce to a task
#' @keywords internal
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

#' Is the object a task?
#' @keywords internal
#' @export
is.task <- function(x) inherits(x, "task")

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

# Load task DESCRIPTION into convenient form.
#' @importFrom stringr str_split str_trim
load_task <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")
  
  if (!file.exists(path_desc)) {
    stop("No task description at ", path_desc, call. = FALSE)
  }
  
  desc <- as.list(read.dcf(path_desc)[1, ])
  desc$path <- path
  
  # Parse comma separated components
  if (!is.null(desc$HitTypeId)) {
    desc$HitTypeId <- str_trim(str_split(desc$HitTypeId, ", ?")[[1]])    
  }

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
  task$HitTypeId <- str_c(task$HitTypeId, collapse = ", ")
  
  dcf <- as.data.frame.list(task)
  stopifnot(nrow(dcf) == 1)
  
  write.dcf(dcf, path)
}

