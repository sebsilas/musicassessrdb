

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

is.scalar.na <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is.scalar.null <- function(x) {
  all(is.null(x)) & length(x) == 0
}

is.scalar.na.or.null <- function(x) {
  is.scalar.na(x) | is.scalar.null(x)
}

is.scalar.na.or.null.or.length.zero <- function(x) {
  is.scalar.na(x) | is.scalar.null(x) | length(x) == 0
}



#' Is NULL or not all TRUE?
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_null_or_not_all_TRUE <- function(x) {
  ! all(x) | is.null(x)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}


is.scalar.na <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is.scalar.null <- function(x) {
  all(is.null(x)) & length(x) == 0
}

is.scalar.na.or.null <- function(x) {
  is.scalar.na(x) | is.scalar.null(x)
}

is.scalar.na.or.null.or.length.zero <- function(x) {
  is.scalar.na(x) | is.scalar.null(x) | length(x) == 0
}


create_session_token <- function(bytes, prefix = "", suffix = "") {
  paste(prefix,
        paste(
          format(as.hexmode(
            sample(256, bytes, replace = TRUE) - 1), width = 2),
          collapse = ""),
        suffix, sep = "")
}

get_nrows <- function(df) {
  # Workaround for backends
  nrows <- df %>%
    dplyr::summarise(num_rows = dplyr::n()) %>%
    dplyr::pull(num_rows) %>%
    as.integer()
}

true_js_to_TRUE <- function(x) {
  if(x == "true") TRUE else if(x == "false") FALSE else stop("Input not recognised.")
}


mutual_column_names <- function(list_of_dataframes) {
  # Extract column names of each dataframe
  column_names <- purrr::map(list_of_dataframes, names)

  # Find the intersection of column names
  mutual_columns <- purrr::reduce(column_names, intersect)

  return(mutual_columns)
}

split_ids <- function(ids) as.integer(strsplit(URLdecode(ids), ",")[[1]])


format_item_ids_in_url <- function() {
  select_items_res <- select_items(2L)
  review_item_ids_str <- paste0(select_items_res$review_items_ids, collapse = ',')
  new_item_ids_str <- paste0(select_items_res$new_items_ids, collapse = ',')
  paste0('new_items_ids=', new_item_ids_str, "&",
         'review_items_ids=', review_item_ids_str)
}

