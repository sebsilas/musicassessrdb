
library(dplyr)
library(purrr)
library(hrep)
library(musicassessrdb)
library(musicassessr)
library(lme4)


item_banks_table_static <- tibble::tibble(
  item_bank_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  item_bank_name = c(
    "Berkowitz_phrase",
    "Berkowitz_ngram",
    "WJD_phrase",
    "WJD_ngram",
    "singpause_item",
    "singpause_phrase",
    "Berkowitz_bottom_10th_percentile",
    "singpause_2024_item",
    "singpause_2024_phrase"
  ),
  item_bank_description = c(
    "The Berkowitz corpus as a phrase item bank.",
    "The Berkowitz corpus as a ngram item bank.",
    "The WJD corpus as a phrase item bank.",
    "The WJD corpus as an N-gram item bank.",
    "SingPause melodies",
    "SingPause melodies, chopped into phrases.",
    "The bottom 10th percentile of the difficulty values for the Berkowitz_ngram item bank",
    "Updated Singpause item item bank for 2024 testing",
    "Updated Singpause phrase item bank for 2024 testing"
  )
)



# ------- DATABASE CONNECTION -------------------- #
db_con <- musicassessr_con()
on.exit({
  logging::loginfo("Disconnecting from DB...")
  DBI::dbDisconnect(db_con)
  logging::loginfo("...disconnected from DB")
})

lambdr::start_lambda()
