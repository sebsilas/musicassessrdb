

split_ids <- function(ids) as.integer(strsplit(URLdecode(ids), ",")[[1]])


format_item_ids_in_url <- function() {
  select_items_res <- select_items(2L)
  review_item_ids_str <- paste0(select_items_res$review_items_ids, collapse = ',')
  new_item_ids_str <- paste0(select_items_res$new_items_ids, collapse = ',')
  paste0('new_items_ids=', new_item_ids_str, "&",
         'review_items_ids=', review_item_ids_str)
}
