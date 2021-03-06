% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_characters.R
\name{get_characters}
\alias{get_characters}
\title{Get information on Marvel characters}
\usage{
get_characters(id = NULL, name = NULL, offset = NULL, limit = NULL)
}
\arguments{
\item{id}{unique character id. Not necessary if name already specified. Optional.}

\item{name}{character name. Not necessary if id already specified. Optional.}

\item{offset}{The requested number of skipped results of the call. Optional.}

\item{limit}{The requested results limit. Optional.}
}
\value{
a dataframe containing the character(s)' information
}
\description{
The get_characters function gets information on Marvel characters
Optionally, users can specify the name or the id (not both) of a character.
Users can also specify the offset and limit of the response
If there is only one requested character, get_characters API will return a dataframe containing the
detailed information of the character. If users request several characters, this API will only
return the brief information on each character.
}
\examples{
get_characters()
get_characters(id = 1011334)
get_characters(offset = 50, limit = 3)
}
