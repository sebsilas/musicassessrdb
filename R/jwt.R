


#' Check JWT
#'
#' @param jwt
#'
#' @return
#' @export
#'
#' @examples
check_jwt <- function(jwt) {

  jwt_file <- Sys.getenv("JWT_KEY")

  jwt <- paste(readLines(jwt_file), collapse = "\n")

  public_key_pem <- jose::jwk_read(jwt)

  tryCatch({

    decoded_jwt <- jose::jwt_decode_sig(jwt_token, public_key_pem)

    tibble::tibble(success = TRUE,
                   message = "The JWT token was valid.")


  }, error = function(err) {

    logging::logerror(err)

    tibble::tibble(success = FALSE,
                   message = "The JWT token was not valid.")

  })


}
