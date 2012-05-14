#' Sign an AWS mturk url.
#' 
#' @section Keys:
#' To use amazon apis you need both an access key (analogous to a username)
#' and a secret key (analogous to a password). Both are available from \url{https://aws-portal.amazon.com/gp/aws/developer/account/index.html?action=access-key#access_credentials}.
#'
#' There are three ways to specify your keys:
#'
#' \itemize{
#'   \item As parameters to this function: \code{access_key},
#'     \code{secret_key}
#'   \item As options, \code{options(AWS_KEY = x)},
#'     \code{options(AWS_SECRET_KEY = y)}
#'   \item As environmental variables: \code{AWS_KEY}, \code{AWS_SECRET_KEY}
#' }
#'
#' If your code is publically available (such as through a public svn or 
#' git repository) DO NOT store your secret in your code, as 
#' this will allow anyone to charge jobs to your amazon account. If you do
#' accidentally publish it, go to the url above, deactivate the old
#' keys and generate new access and secret keys.
#'
#' @param access_key your amazon access key
#' @param secret_key your amazon secret key
sign_mturk <- function(service, access_key = NULL, secret_key = NULL) {
  config(signature = function(method, url) {
    url <- sign_aws(url, service = service, access_key = access_key, 
      secret_key = secret_key)
    list(url = build_url(url), config = config())
  })
}


aws_signature <- function(url, access_key = NULL, secret_key = NULL) {
  url <- parse_url(url)
  
  access_key <- access_key %||% getOption("AWS_KEY") %||%
    Sys.getenv("AWS_KEY")
  secret_key <- secret_key %||% getOption("AWS_SECRET_KEY") %||%
    Sys.getenv("AWS_SECRET_KEY")
  time <- timestamp()
  op <- url$query$operation
  
  service <- "AWSMechanicalTurkRequester"
  sig_val <- str_c(service, op, timestamp)
  sig <- hmac_sha1(secret_key, sig_val)

  url$query$AWSAccessKeyId <- access_key
  url$query$Timestamp <- time
  url$query$Signature <- sig
  
  url
}



