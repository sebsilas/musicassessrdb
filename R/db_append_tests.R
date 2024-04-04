


#' Append an experiment to the tests table
#'
#' @param db_con The DB connection.
#' @param test_name The name of the test.
#' @param test_description A description of the test.
#' @param active
#'
#' @return
#' @export
#'
#' @examples
db_append_tests <- function(db_con, test_name, test_description, active = TRUE) {

  stopifnot(
    is.scalar.character(test_name),
    is.scalar.character(test_description),
    is.scalar.logical(active)
  )


  tests_df <- tibble::tibble(test_name = test_name,
                             test_description = test_description,
                             active = active)

  test_id <- db_append_to_table(db_con, table = "tests", data = tests_df, primary_key_col = "test_id")

  return(test_id)
}


