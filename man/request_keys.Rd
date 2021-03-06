% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request_keys.R
\name{request_keys}
\alias{request_keys}
\title{Request API keys}
\usage{
request_keys()
}
\value{
a list containing private key and public key
}
\description{
This function requests API keys from users' R environment.
If keys are not set, the function terminates and prompts the users to set API keys in the R
environment file. Specifically, user_renviron = path.expand(file.path('~', '.Renviron')).
If the path doesn't exist, execute the following code: file.create(user_renviron)
If the path exists, excute the following code: file.edit(user_renviron)
}
\examples{
request_keys()
}
