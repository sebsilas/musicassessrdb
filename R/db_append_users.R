
#' Append a user to the users table
#'
#' @param db_con
#' @param username
#' @param password
#' @param enabled
#' @param created_at
#'
#' @return
#' @export
#'
#' @examples
db_append_users <- function(db_con, username, password, enabled = TRUE, created_at = Sys.time() ) {

  stopifnot(
    is.scalar.character(username),
    is.scalar.character(password),
    is.scalar.logical(enabled),
    is(created_at, "POSIXct")
  )


  users_df <- tibble::tibble(username = username,
                             password = digest::hmac(
                               key = Sys.getenv(x = "ENCRYPTION_KEY"),
                               object = password,
                               algo = "sha512"
                             ),
                             enabled = enabled,
                             created_at = created_at)

  db_append_to_table(db_con, table = "users", data = users_df)
}
