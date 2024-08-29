
feedback <- function(Records) {

  logging::loginfo("Inside feedback function")

  processed_file <- jsonlite::fromJSON(Records$body)[[1]][[1]][[9]][[4]][[1]][[1]]
  logging::loginfo("processed_file = %s", processed_file)

  user_input_as_pyin <- readFromS3(filename = processed_file, bucket = Sys.getenv("DESTINATION_BUCKET"))

  # Return response

  response <- tryCatch({


    if(type == "opti3") {
      result <- get_opti3(stimuli, stimuli_durations, length(stimuli), user_input_as_pyin)
    }



    list(
      status = 200,
      message = "You have successfully got trial feedback!",
      result = result
    )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "There was an error getting trial feedback!",
      result = NA
    )

  })

  return(response)

}
