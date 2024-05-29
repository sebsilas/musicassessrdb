



check_jwk <- function(jwt_token) {

  jwk_file <- Sys.getenv("JWK_KEY")

  jwk <- paste(readLines(jwk_file), collapse = "\n")

  public_key_pem <- jose::jwk_read(jwk)

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
