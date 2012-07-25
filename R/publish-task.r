#' Publish all templated HITs.
#' 
#' If present, a column in the template called \code{max_assignments} will be
#' used to set the maximum number of assignments for each HIT.
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
#' @return Invisibly returns new HIT ids
#' @export
publish_task <- function(task = NULL, ..., quiet = FALSE) {
  task <- as.task(task)
    
  hit_type_id <- register_task(task, ...)
  
  # Load template, and restrict to HITs that haven't been created
  template <- load_template(task)
  if (is.null(template$hit_id)) template$hit_id <- NA
  rows <- which(is.na(template$hit_id))
  n <- length(rows)

  n_published <- sum(!is.na(template$hit_id))
  if (!quiet && n_published > 0)
    message(n_published, " HITs already published")
  
  if (length(n) == 0) return()
  
  max_assignments <- template$max_assignments %||% rep(1, nrow(template))
  lifetime <- parse_duration(task$OverallTimeLimit)

  templates <- render_templates(task, data = template[rows, , drop = FALSE])
  
  for(i in seq_along(templates)) {
    row <- rows[i]
    if (!quiet) {
      message("Publishing HIT [", i, "/", n, "]")
    }

    xml <- mturk_task_req(task, "CreateHIT",
      HITTypeId = hit_type_id, 
      Question = templates[[i]], 
      LifetimeInSeconds = lifetime,
      MaxAssignments = max_assignments[row], ...)
    hit_id <- xmlValue(xml[["HIT"]][["HITId"]][[1]])
    template$hit_id[row] <- hit_id
    write.csv(template, file.path(task$path, "template.csv"), 
      row.names = FALSE)
  }
  
  invisible(template$hit_id)
}

#' Unpublish all published HITs.
#'
#' This operation "removes a HIT from the Amazon Mechanical Turk marketplace,
#' approves all submitted assignments that have not already been approved or
#' rejected, and disposes of the HIT and all assignment data.".
#'
#' In MTurk API terminology this is disabling the HIT.
#' 
#' This function removes both HITs listed in the template ("linked" HITs) and
#' other HITs associated with this task that are on MTurk, but not recorded in
#' the csv file ("unlinked" HITs).
#'
#' @inheritParams publish_task
#' @return Invisibly returns disabled HIT ids
#' @export
unpublish_task <- function(task, ..., quiet = FALSE) {
  task <- as.task(task)
  
  template <- load_template(task)
  rows <- which(!is.na(template$hit_id))
  ids <- template$hit_id[rows]
  
  n <- length(ids)
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    if (!quiet) {
      message("Unpublishing linked HIT [", i, "/", n, "] ",
        template$hit_id[row])
    }
    mturk_task_req(task, "DisableHIT", HITId = template$hit_id[row], ...)
    template$hit_id[row] <- NA
    write.csv(template, file.path(task$path, "template.csv"), 
      row.names = FALSE)
  }
  
  others <- list_hits(task, ...)
  n <- length(others)
  for (i in seq_along(others)) {
    id <- others[[i]]
    if (!quiet) {
      message("Unpublishing unlinked HIT [", i, "/", n, "] ", id)
    }
    mturk_task_req(task, "DisableHIT", HITId = id, ...)
  }
  
  invisible(rows)
}
