

load_all()

# db_con <- musicassessr_con()

inst_table_db <- musicassessrdb::get_table(db_con, "instruments") %>%
  dplyr::collect()

item_banks_table_static <- musicassessrdb::get_table(db_con, "item_banks") %>%
  dplyr::collect()

db_disconnect(db_con)

use_data(inst_table_db, internal = TRUE, overwrite = TRUE)

use_data(item_banks_table_static, overwrite = TRUE)

document()

