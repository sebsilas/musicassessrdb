

# db_con <- musicassessr_con()

get_singpause_items_v2 <- function(user_id = NULL) {


  item_bank <- dplyr::tbl(db_con, "item_bank_singpause_2025_phrase") %>%
    dplyr::select(
      item_id,
      song_name,
      image,
      audio_file,
      lyrics_file,
      abs_melody,
      durations,
      phrase_name
    ) %>%
    dplyr::collect()

  trials <- compile_item_trials(db_con, user_id = user_id, add_trial_scores = TRUE) %>%
    dplyr::filter(grepl("singpause_2025", item_id)) %>%
    dplyr::mutate(Date = lubridate::as_date(trial_time_completed)) %>%
    dplyr::rename(score = opti3) %>%
    dplyr::select(Date, item_id, score) %>%
    dplyr::group_by(item_id) %>%
    dplyr::slice_max(Date) %>% # Get latest score
    dplyr::ungroup()

  item_bank <- item_bank %>% dplyr::left_join(trials, by = "item_id")

  # Now group by item_id
  grouped <- item_bank %>%
    dplyr::group_by(song_name) %>%
    dplyr::summarise(
      phrases = list(phrase_name),
      audio_file = list(audio_file),
      lyrics_file = list(lyrics_file),
      abs_melody = list(abs_melody),
      durations = list(durations),
      scores = list(score),
      .groups = "drop"
    )

  return(grouped)
}


# t <- get_singpause_items_v2(138L)

