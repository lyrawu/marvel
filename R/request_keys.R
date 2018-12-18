#' Request API keys
#'
#' This function requests API keys from users' R environment.
#' If keys are not set, the function terminates and prompts the users to set API keys in the R
#' environment file. Specifically, user_renviron = path.expand(file.path('~', '.Renviron')).
#' If the path doesn't exist, execute the following code: file.create(user_renviron)
#' If the path exists, excute the following code: file.edit(user_renviron)
#'
#' @return a list containing private key and public key
#' @examples
#' request_keys()
#' @export


request_keys <- function() {
  public_key <- Sys.getenv("MARVEL_API_PUBLIC_KEY")
  private_key <- Sys.getenv("MARVEL_API_PRIVATE_KEY")
  if (identical(public_key, "")) {
    stop("`MARVEL_API_PUBLIC_KEY` env var has not been set. Run 'file.edit(path.expand(file.path('~', '.Renviron')))' to edit.")
  }
  if (identical(private_key, "")) {
    stop("`MARVEL_API_PRIVATE_KEY` env var has not been set. Run 'file.edit(path.expand(file.path('~', '.Renviron')))' to edit.")
  }
  return(list(private_key, public_key))
}
