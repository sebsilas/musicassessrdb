

db_con <- musicassessr_con()

inst_table_db <- musicassessrdb::get_table(db_con, "instruments") %>%
  dplyr::collect()

item_banks_table_static <- musicassessrdb::get_table(db_con, "item_banks") %>%
  dplyr::collect()

DBI::dbDisconnect(db_con)

use_data(inst_table_db, internal = TRUE)

use_data(item_banks_table_static)
