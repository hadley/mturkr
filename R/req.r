#' @importFrom stringr str_c
mturk_req_url <- function(host = "sandbox", ...) {
  host <- match.arg(host, c("sandbox", "production"))
  host2 <- c(
    "sandbox" = "mechanicalturk.sandbox.amazonaws.com",
    "production" = "mechanicalturk.amazonaws.com"
  )[host]
  
  base <- str_c("https://", host2)
  
  params <- c(...,
    Service = "AWSMechanicalTurkRequester",
    Version = "2008-08-02"
  )
  
  params <- vapply(params, url_encode, FUN.VALUE = character(1))
  
  str_c("https://", host2, "?", 
    str_c(names(params), "=", params, collapse = "&"))
}

#' @importFrom stringr str_wrap
mturk_req <- function(host = "sandbox", operation, ...) {
  url <- mturk_req_url(host = host, Operation = operation, ...)
  # message(url)
  
  result <- getURL(url)
  xml <- xmlTreeParse(result)$doc$children[[1]]
  
  # Find and report on all errors
  aws_xml_error(getNodeSet(xml, "//Error"))
  
  xml
}

aws_xml_error <- function(errors) {
  if (length(errors) == 0) return()
  
  err_string <- function(err) {
    code <- xmlValue(err[["Code"]][[1]])
    msg <- xmlValue(err[["Message"]][[1]])

    str_wrap(str_c("[", code, "] ", msg), exdent = 2)
  }
  
  errs <- vapply(errors, err_string, character(1))
  stop(str_c(errs, collapse = "\n\n"), call. = FALSE)
  
}

#' Perform operation for given task.
#' 
#' @section Keys:
#' To use amazon mturk you need both an access key (analogous to a username)
#' and a secret key (analogous to a password). Both are available from \url{https://aws-portal.amazon.com/gp/aws/developer/account/index.html?action=access-key#access_credentials}.
#'
#' There are three ways to specify your keys:
#'
#' \itemize{
#'   \item As parameters to any request function: \code{access_key},
#'     \code{secret_key}
#'   \item As parameters in your task description: \code{AccessKey},
#'     \code{SecretKey}
#'   \item As environmental variables: \code{AWS_KEY}, \code{AWS_SECRET_KEY}
#' }
#'
#' If your code is publically available (such as through a public svn or 
#' git repository) DO NOT store your secret in your task description, as 
#' this will allow anyway to charge jobs to your amazon account. If you do
#' accidentally publish it, go to the url above and deactivate it and 
#' generate new access and secret keys.
#'
#' @return an XML object, \code{\link{XMLInternalDocument-class}}
mturk_task_req <- function(task, operation, host = NULL, access_key = NULL, secret_key = NULL, ...) {
  task <- as.task(task)
  
  host <- host %||% task$Host
  access_key <- access_key %||% task$AccessKey %||% Sys.getenv("AWS_KEY")

  secret_key <- secret_key %||% task$SecretKey %||%
   Sys.getenv("AWS_SECRET_KEY")
  time <- timestamp()
  sig <- make_signature(secret_key, operation, time)
  
  mturk_req(host = host, AWSAccessKeyId = access_key, operation = operation, 
    Timestamp = time, Signature = sig, ...)
}


#' Make HMAC encoded signature for all AWS requests.
#' 
#' @importFrom digest hmac
#' @importFrom stringr str_c
#' @importFrom RCurl base64
#' @return base64 encoded signature
make_signature <- function(secret_key, operation, timestamp) {
  service <- "AWSMechanicalTurkRequester"
  
  sig_val <- str_c(service, operation, timestamp)
  hmac_sha1(secret_key, sig_val)
}

hmac_sha1 <- function(key, string) {
  hash <- hmac(key, string, "sha1", raw = TRUE)
  base64(hash)
}


url_encode <- function(x) {
  stopifnot(length(x) == 1)
  if (is.na(x)) return(NA_character_)
  
  percent_encode <- function(x) {
    raw <- vapply(x, function(x) as.character(charToRaw(x)), character(1))
    str_c("%", raw)
  }

  letters <- strsplit(x, "")[[1]]
  not_ok <- str_detect(letters, "[^-A-Za-z0-9_.~]")

  letters[not_ok] <- percent_encode(letters[not_ok])
  str_c(letters, collapse = "")
}
