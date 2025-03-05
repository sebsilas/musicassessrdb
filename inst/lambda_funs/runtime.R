
library(dplyr)
library(purrr)
library(hrep)
library(musicassessrdb)
library(musicassessr)
library(lme4)


item_banks_table_static <- musicassessrdb::item_banks_table_static


# ------- DATABASE CONNECTION -------------------- #

if(Sys.getenv("DISABLE_POOL") == "TRUE") {
  db_con <- musicassessr_con(pool = FALSE)
} else {
  db_con <- musicassessr_con()
}

on.exit({
  db_disconnect(db_con)
  logging::loginfo("...disconnected from DB")
})

lambdr::start_lambda()
