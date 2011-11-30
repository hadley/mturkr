#' Register a task (HITType).
#' 
#' Register your task as a HITType on MTurk. The HIT id is stored in your
#' task DESCRIPTION file so that there's always a record of which HIT types
#' have been generated from this task - there may be multiple if you have
#' changed the title, description, keywords etc.
#'
#' If this task has been registered previously, and you have changed any of 
#' the HIT type metadata, you'll get a warning notification.
#'
#' @param The location of an mturk task, see \code{\link{as.task}} for
#'   specification options
#' @param ... Other parameters passed on to \code{\link{mturk_task_req}}.
#' @return the HIT type ID
#' @export
register_task <- function(task = NULL, ...) {
  task <- as.task(task)
  
  # qual <- parse_qualification(task$Qualifications)
  # duration <- parse_duration(task$TimeLimits)
  # reward <- parse_reward(task$TimeLimits)

  result <- mturk_task_req(task, "RegisterHITType", 
    Title = task$Title,
    Description = task$Description,
    Keywords = task$Keywords,
    # QualificationRequirement = qual,
    # Reward = reward,
    # AssignmentDurationInSeconds = duration,
    Reward.1.Amount = 0.01,
    Reward.1.CurrencyCode = "USD",
    AssignmentDurationInSeconds = 60 * 60 * 24,
    Reward = 
    ...)
    
  id <- xmlValue(result[["HITTypeId"]][[1]])
  existing <- task$HitTypeId
  if (!(id %in% existing)) {
    if (length(existing) > 0) {
      warning("Creating new HIT type id: ", id, call. = FALSE)
    }
    task$HitTypeId <- c(id, existing)
  }
  
  save_task(task)
  
  id
}
