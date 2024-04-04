


# Test

# dem_res <- readRDS("~/musicassessrdb/output/results/id=1&p_id=3ed07192fb6631d3cdd5384a4093478a77b060cb89975d489272798ec997ba9a&save_id=1&pilot=false&complete=true.rds") %>%
#   as.data.frame() %>%
#   tidyr::unnest_wider(DEG.q9, names_sep = "_") %>%
#   tidyr::unnest_wider(DEG.Handedness, names_sep = "_")


# t <- db_append_demographics(db_con, user_id = 1L, demographics_df = dem_res)

# API test

# t <- store_db_demographics_api(1L, dem_res)




# curl -X POST 'https://kuqchtwsfb.execute-api.us-east-1.amazonaws.com/append-demographics' \
# -H 'Content-Type: application/json' \
# -d '{
#   "user_id": 1,
#   "demographics_list": {
#     "session.p_id": "3ed07192fb6631d3cdd5384a4093478a77b060cb89975d489272798ec997ba9a",
#     "session.pilot": false,
#     "session.complete": true,
#     "session.time_started": 1700849726.00126,
#     "session.current_time": 1700849787.09606,
#     "session.num_restarts": 0,
#     "session.language": "en",
#     "DEG.q1": "btn1_text",
#     "DEG.q2": "btn2_text",
#     "DEG.q4": "btn1_text",
#     "DEG.q5": "UK",
#     "DEG.q6": "UK",
#     "DEG.q7": "en",
#     "DEG.q8": "none",
#     "DEG.q9_1": "2",
#     "DEG.q9_2": "2012",
#     "DEG.q10": "btn1_text",
#     "DEG.q11": "btn2_text",
#     "DEG.Best Shot": 1,
#     "DEG.Hearing Impairment": 2,
#     "DEG.Gender": 1,
#     "DEG.Age": 141,
#     "DEG.Nationality": "UK",
#     "DEG.Country Formative Years": "UK",
#     "DEG.First Language": "en",
#     "DEG.Second Language": "none",
#     "DEG.Handedness_1": 1,
#     "DEG.Handedness_2": 2,
#     "SES.q1": "choice4",
#     "SES.q2": "choice4",
#     "SES.q3": "choice1",
#     "SES.q5": "choice1",
#     "SES.q6": "choice1",
#     "SES.educational_degree": 3,
#     "SES.class": 1,
#     "GMS.q1": "btn1_text",
#     "GMS.q2": "btn4_text",
#     "GMS.q3": "btn2_text",
#     "GMS.q4": "btn3_text",
#     "GMS.q5": "btn3_text",
#     "GMS.q6": "btn3_text",
#     "GMS.q7": "btn3_text",
#     "GMS.q8": "btn3_text",
#     "GMS.q9": "btn3_text",
#     "GMS.q10": "btn3_text",
#     "GMS.q11": "btn3_text",
#     "GMS.q12": "btn6_text",
#     "GMS.q13": "btn2_text",
#     "GMS.q14": "btn2_text",
#     "GMS.q15": "btn3_text",
#     "GMS.q16": "btn4_text",
#     "GMS.q17": "btn5_text",
#     "GMS.q18": "btn6_text",
#     "GMS.q19": "btn1_text",
#     "GMS.q20": "btn2_text",
#     "GMS.q21": "btn6_text",
#     "GMS.q22": "btn7_text",
#     "GMS.q23": "btn1_text",
#     "GMS.q24": "btn2_text",
#     "GMS.q25": "btn3_text",
#     "GMS.q26": "btn4_text",
#     "GMS.q27": "btn2_text",
#     "GMS.q28": "btn1_text",
#     "GMS.q29": "btn3_text",
#     "GMS.q30": "btn4_text",
#     "GMS.q31": "btn5_text",
#     "GMS.q32": "btn6_text",
#     "GMS.q33": "btn2_text",
#     "GMS.q34": "btn1_text",
#     "GMS.q35": "btn2_text",
#     "GMS.q36": "btn3_text",
#     "GMS.q37": "btn4_text",
#     "GMS.q38": "btn3_text",
#     "GMS.q39": "btn2_text",
#     "GMS.q40": "btn11_text",
#     "GMS.q41": "btn1_text",
#     "GMS.Active Engagement": 3.44444444444444,
#     "GMS.General": 3.66666666666667,
#     "GMS.Musical Training": 5.57142857142857,
#     "GMS.Emotions": 3,
#     "GMS.Singing Abilities": 3.28571428571429,
#     "GMS.Perceptual Abilities": 3.11111111111111,
#     "GMS.Instrument": 6,
#     "GMS.Start Age": 11,
#     "GMS.Absolute Pitch": 1
#   }
# }'



store_db_demographics_api <- function(user_id, demographics_df) {

  logging::loginfo("store_db_demographics_api")
  logging::loginfo('dim(demographics_df)')
  logging::loginfo(dim(demographics_df))

  stopifnot(
    is.data.frame(demographics_df),
    dim(demographics_df)[1] == 1L,
    dim(demographics_df)[2] %in% c(84L, 85L, 86L)
  )

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       demographics = as.list(demographics_df))

  endpoint_wrapper(function_name = "append-demographics",
                   request_body = request_body)

}






# This is the function that is called when the endpoint
# is invoked
append_demographics <- function(user_id, demographics_list) {

  logging::loginfo("Appending to demographics table")

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("demographics_df = %s", demographics_list)

  demographics_df <- tibble::as_tibble(demographics_list)

  # Make sure this is done before the following step:

  demographics_df[demographics_df == 987654321] <- NA

  if(!'DEG.q3' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(DEG.q3 = NA_character_)
  }

  if(!'SES.q4' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(SES.q4 = NA_character_)
  }

  if(!'SES.q5' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(SES.q5 = NA_character_)
  }


  # Return response

  response <- tryCatch({

    # Append condition

    db_append_demographics(db_con, user_id, demographics_df)

    list(
      status = 200,
      message = paste0("You have successfully added an entry to the demographic table for user_id ", user_id, "!")
      )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      condition_id = NA
    )

  })

  return(response)

}




#' Append a condition to the conditions table
#'
#' @param db_con The DB connection.
#' @param user_id The user ID.
#' @param demographics_df A 1 x 86 dataframe of the demographic information.
#'
#' @return
#' @export
#'
#' @examples
db_append_demographics <- function(db_con, user_id, demographics_df) {

  logging::loginfo('In db_append_demographics')

  logging::loginfo('dim(demographics_df)')
  logging::loginfo(dim(demographics_df))

  stopifnot(
    is.integer(user_id),
    is.data.frame(demographics_df),
    dim(demographics_df)[1] == 1L,
    dim(demographics_df)[2] %in% c(85L, 86L, 87L)
  )


  tryCatch({

    now <- as.POSIXct(format(Sys.time()),tz="UTC") # Timezone must be in UTC

    demographics_df <- cbind(tibble::tibble(user_id = user_id, time_added = now ), demographics_df) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        session.time_started = lubridate::as_datetime(session.time_started),
        session.current_time = lubridate::as_datetime(session.current_time)
      )

    check_id_exists(db_con, table_name = "users", id_col = "user_id", id = user_id)

    db_append_to_table(db_con, table = "demographics", data = demographics_df, primary_key_col = "user_id")


  }, error = function(err) {
    logging::logerror(err)
  })



  return(user_id)
}
