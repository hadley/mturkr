"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_time <- function(x) {
  as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}
