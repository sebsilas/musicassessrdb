

#' Append a user to the users table
#'
#' @param db_con
#' @param dev_vs_prod
#' @param username
#' @param password
#' @param enabled
#' @param created_at
#' @param email
#' @param app_name
#'
#' @return
#' @export
#'
#' @examples
db_append_users <- function(db_con = NULL,
                            dev_vs_prod = c("dev", "prod"),
                            username,
                            password = NA_character_,
                            enabled = TRUE,
                            created_at = Sys.time(),
                            email = NA_character_,
                            app_name = NA_character_) {

  dev_vs_prod <- match.arg(dev_vs_prod)

  stopifnot(
    is.scalar.character(username),
    dev_vs_prod %in% c("dev", "prod"),
    is.scalar.character(password),
    is.scalar.logical(enabled),
    is(created_at, "POSIXct")
  )


  if(is.null(db_con)) {
    if(dev_vs_prod == "dev") {
      db_con <- musicassessrdb::musicassessr_con(db_name = "melody_dev")
    } else {
      db_con <- musicassessrdb::musicassessr_con(db_name = "melody_prod")
    }
  }

  users <- dplyr::tbl(db_con, "users") %>%
    dplyr::filter(username == !! username) %>%
    dplyr::collect()

  if(nrow(users) > 1L) {
    stop("User already exists!")
  }

  users_df <- tibble::tibble(username = username,
                             password = NA,
                             enabled = enabled,
                             created_at = created_at,
                             email = email,
                             app_name = app_name)

  db_append_to_table(db_con, table = "users", primary_key_col = "user_id", data = users_df)
}


# t <- db_append_users(dev_vs_prod = "dev", username = "singpause_saa_pretest")

#
