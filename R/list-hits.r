#' List all HITs associated with this task.
#' 
#' Works by retrieving all HIT associated with user, and then filtering to 
#' match the HIT type ids listed in the DESCRIPTION.
#' 
#' @inheritParams publish_task
#' @export
list_hits <- function(task, ...) {
  task <- as.task(task)
  
  xml <- mturk_task_req(task, "SearchHITs", ...)
  hits <- getNodeSet(xml, "//HIT")
  
  ids <- vapply(hits, function(x) xmlValue(x[["HITId"]][[1]]), character(1))
  type_ids <- vapply(hits, function(x) xmlValue(x[["HITTypeId"]][[1]]),
    character(1))
    
  ids[type_ids %in% task$HitTypeId]
}
