

db_con <- musicassessr_con()

inst_table_db <- musicassessrdb::get_table(db_con, "instruments") %>%
  dplyr::collect()

DBI::dbDisconnect(db_con)

use_data(inst_table_db, internal = TRUE)
