


#' Check JWT
#'
#' @param jwt
#'
#' @return
#' @export
#'
#' @examples
check_jwt <- function(jwt) {

  jwk_file <- Sys.getenv("JWK_KEY")

  jwk <- paste(readLines(jwk_file), collapse = "\n")

  public_key_pem <- jose::jwk_read(jwk)

  tryCatch({

    decoded_jwt <- jose::jwt_decode_sig(jwt, public_key_pem)


    tibble::tibble(success = TRUE,
                   message = "The JWT token was valid.",
                   username = decoded_jwt$cognito:username,
                   user_id = as.integer(decoded_jwt$custom:userId)


  }, error = function(err) {

    logging::logerror(err)

    tibble::tibble(success = FALSE,
                   message = "The JWT token was not valid.",
                   username = NA,
                   user_id = NA)

  })


}
