
library(dplyr)
library(purrr)
library(hrep)
library(musicassessrdb)
library(musicassessr)


# ------- DATABASE CONNECTION -------------------- #
db_con <- musicassessr_con()
on.exit({
  logging::loginfo("Disconnecting from DB...")
  DBI::dbDisconnect(db_con)
  logging::loginfo("...disconnected from DB")
})

lambdr::start_lambda()
