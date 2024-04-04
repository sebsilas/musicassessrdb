
#' Get tests or experiments available to a user
#'
#' @param db_con Db connection
#' @param user_id User ID
#'
#' @return Data.frame with id and name
#' @export
get_available <- function(db_con, user_id, type = c("tests", "experiments")) {

  type <- match.arg(type)

  stopifnot(
    type %in% c("tests", "experiments")
  )


  if(type == "tests") {
    id <- as.name("test_id")
    name <- as.name("test_name")
  } else {
    id <- as.name("experiment_id")
    name <- as.name("experiment_name")
  }

  # Get groups user belongs to

  groups <- get_user_groups(db_con, user_id)

  # Fetch group permissions
  group_raw <- dplyr::tbl(db_con, paste0(type, "_", "group_permissions")) %>%
    dplyr::filter(user_id %in% !! user_id) %>%
    dplyr::collect()

  group_allowed <- group_raw %>%
    dplyr::filter(action == "Allow") %>%
    dplyr::pull(!!id) %>%
    unique()

  group_denied <- group_raw %>%
    dplyr::filter(action == "Deny") %>%
    dplyr::pull(!!id) %>%
    unique()

  # Fetch user permissions
  user_raw <- dplyr::tbl(db_con, paste0(type, "_", "user_permissions")) %>%
    dplyr::filter(user_id == !!user_id) %>%
    dplyr::collect()

  user_allowed <- user_raw %>%
    dplyr::filter(action == "Allow") %>%
    dplyr::pull(!!id) %>%
    unique()

  user_denied <- user_raw %>%
    dplyr::filter(action == "Deny") %>%
    dplyr::pull(!!id) %>%
    unique()

  # Combine, respecting the priority of user permissions
  ids <- group_allowed %>%
    setdiff(group_denied) %>%
    c(user_allowed) %>%
    setdiff(user_denied)

  dplyr::tbl(db_con, type) %>%
    dplyr::filter(!!id %in% ids,
                  active) %>%
    dplyr::collect()
}



get_user_groups <- function(db_con, user_id) {

  user_groups <- dplyr::tbl(db_con, "users_groups") %>%
    dplyr::filter(user_id == !!user_id) %>%
    dplyr::select(user_id, group_id) %>%
    dplyr::collect()

  if(nrow(user_groups) < 1) {

    logging::logwarn("No user groups, returning empty result")

    empty_result <- tibble::tibble(
      user_id = integer(0),
      group_id = character(0)
    )
    return(user_groups)
  }

  user_groups

}

#' Give a specific user permissions to a test or experiment.
#'
#' @param db_con
#' @param user_id
#' @param id The test or experiment ID.
#' @param type
#' @param action
#'
#' @return
#' @export
#'
#' @examples
give_user_permissions <- function(db_con, user_id, id, type = c('tests', 'experiments'), action = c('Allow', 'Deny')) {

  type <- match.arg(type)
  action <- match.arg(action)

  if(type == "test") {
    tb <- tibble::tibble(test_id = id, action = action, user_id = user_id)
    ret <- db_append_to_table(db_con, table = "tests_user_permissions", data = conditions_df, primary_key_col = "user_id")
  } else if(type == "experiments") {
    tb <- tibble::tibble(experiment_id = id, action = action, user_id = user_id)
    ret <- db_append_to_table(db_con, table = "experiments_user_permissions", data = conditions_df, primary_key_col = "user_id")
  } else {
    stop("False type.")
  }

  return(ret)

}


#' Give a specific user permissions to a test or experiment.
#'
#' @param db_con
#' @param user_id
#' @param id The test or experiment ID.
#' @param type
#' @param action
#'
#' @return
#' @export
#'
#' @examples
give_group_permissions <- function(db_con, group_id, id, type = c('tests', 'experiments'), action = c('Allow', 'Deny')) {

  type <- match.arg(type)
  action <- match.arg(action)

  if(type == "test") {
    tb <- tibble::tibble(test_id = id, action = action, group_id = group_id)
    ret <- db_append_to_table(db_con, table = "tests_group_permissions", data = conditions_df, primary_key_col = "user_id")
  } else if(type == "experiments") {
    tb <- tibble::tibble(experiment_id = id, action = action, group_id = group_id)
    ret <- db_append_to_table(db_con, table = "experiments_group_permissions", data = conditions_df, primary_key_col = "user_id")
  } else {
    stop("False type.")
  }
  return(ret)

}

add_user_to_group <- function(db_con, user_id, group_id) {
  db_append_to_table(db_con, table = "users_groups", data = tibble::tibble(user_id = user_id, group_id = group_id), primary_key_col = "user_group_id")
}

give_user_public_permissions <- function(db_con, user_id, public_id = 2L) {
  logging::loginfo("Giving user %s public permissions.", user_id)
  add_user_to_group(db_con, user_id, public_id)
}

# db_con <- musicassessr_con()

# t <- get_user_groups(db_con, user_id = 1L)

# t <- get_available(db_con, user_id = 3L, type = "tests")
