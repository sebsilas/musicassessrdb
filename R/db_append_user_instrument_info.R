

#' Append user instrument information the the user_instrument_info table
#'
#' @param db_con
#' @param user_id
#' @param instrument_id
#' @param bottom_range
#' @param top_range
#'
#' @return
#' @export
#'
#' @examples
db_append_user_instrument_info <- function(db_con, user_id, instrument_id, bottom_range, top_range) {

  stopifnot(
    is.integer(user_id),
    is.integer(instrument_id),
    is.integer(bottom_range) & bottom_range %in% midi.gamut,
    is.integer(top_range) & top_range %in% midi.gamut
  )


  instrument_ranges_df <- tibble::tibble(
    user_id = user_id,
    instrument_id = instrument_id,
    bottom_range = bottom_range,
    top_range = top_range)

  db_append_to_table(db_con, table = "user_instrument_info", data = instrument_ranges_df)
}
