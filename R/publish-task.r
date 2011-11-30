#' Publish all templated HITs.
#' 
#' If present, a column in the template called \code{max_assignments} will be
#' used to set the maximum number of assingments for each HIT.
#'
#' @inheritParams register_task
#' @section Required metadata:
#'
#' \itemize{
#'   \item \code{OverallTimeLimit}: Maximum amount of time HIT is available
#'     for. Can be an integer (\code{1}), or can specify seconds, minutes,
#'     hours, days or weeks (or any unique prefix): \code{40 seconds}, 
#'    \code{5 minutes}, \code{1 day}, \code{2 w}.
#' }
#' @export
publish_task <- function(task = NULL, ..., quiet = FALSE) {
  task <- as.task(task)
    
  hit_type_id <- register_task(task)
  
  # Load template, and restrict to HITs that haven't been created
  template <- load_template(task)
  if (is.null(template$hit_id)) template$hit_id <- NA
  rows <- which(is.na(template$hit_id))
  n <- length(rows)
  
  if (!quiet)
    message(sum(!is.na(template$hit_id)), " HITs already published")
  if (length(n) == 0) return()
  
  max_assignments <- template$max_assignments %||% rep(1, nrow(template))
  lifetime <- parse_duration(task$OverallTimeLimit)

  templates <- render_templates(task, data = template[rows, , drop = FALSE])
  
  for(i in seq_along(templates)) {
    row <- rows[i]
    if (!quiet) {
      message("Creating HIT [", i, "/", n, "]")
    }

    xml <- mturk_task_req(task, "CreateHIT",
      HITTypeId = hit_type_id, 
      Question = templates[[i]], 
      LifetimeInSeconds = lifetime,
      MaxAssignments = max_assignments[row])
    hit_id <- xmlValue(xml[["HIT"]][["HITId"]][[1]])
    template$hit_id[row] <- hit_id
    write.csv(template, file.path(task$path, "template.csv"), 
      row.names = FALSE)
  }
  
  invisible(template$hit_id)
}
