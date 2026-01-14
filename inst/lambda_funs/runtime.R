
Sys.setenv(RETICULATE_PYTHON = "/opt/pyenv/bin/python")

if (Sys.getenv("LOAD_PYTHON", "TRUE") == "TRUE") {
  message("Initializing Python environment...")
  library(reticulate)
  reticulate::use_python("/opt/pyenv/bin/python", required = TRUE)

  cfg <- reticulate::py_config()
  message(sprintf("Python: %s", cfg$python))
  message(sprintf("Transformers available? %s",
                  reticulate::py_module_available("transformers")))
}

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
