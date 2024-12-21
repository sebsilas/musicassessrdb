
#
endpoint_wrapper <- function(function_name, request_body, endpoint_url = Sys.getenv("ENDPOINT_URL")) {

  endpoint <- paste0(endpoint_url, function_name)

  headers <- httr::add_headers("content type" = "application/json")

  # Send the POST request
  response <- httr::POST(endpoint, body = request_body, encode = "json", headers = headers)

  # Print the response
  logging::loginfo("Response: %s", httr::content(response, encoding = "UTF-8"))

  if(httr::status_code(response) == 200) {
    result <- response %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
  } else {
    result <- NA
  }

}
