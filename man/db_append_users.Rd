% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_append_users.R
\name{db_append_users}
\alias{db_append_users}
\title{Append a user to the users table}
\usage{
db_append_users(
  db_con = NULL,
  dev_vs_prod = c("dev", "prod"),
  username,
  password = NA_character_,
  enabled = TRUE,
  created_at = Sys.time(),
  email = NA_character_,
  app_name = NA_character_
)
}
\arguments{
\item{app_name}{}
}
\description{
Append a user to the users table
}
