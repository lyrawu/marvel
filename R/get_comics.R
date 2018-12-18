#' Get information on Marvel comics.
#'
#' The get_comics API returns information on Marvel comics
#' Optionally, users can specify the ID of a comics.
#' Users can also specify the offset and limit of the response.
#' If there is only one returned character, get_comics API will return a dataframe containing the
#' detailed information of the comics. If there are several returned comics, this API will only
#' return brief information on each comics.
#'
#' @param id unique comics id. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the comics' information
#' @examples
#' get_comics()
#' get_comics(id = 59548)
#' get_comics(offset = 50, limit = 3)
#' @export

get_comics = function(id = NULL, offset = NULL, limit = NULL){
  # Construct query parameters
  key_list = marvel::request_keys()
  private_key = key_list[[1]][1]
  public_key = key_list[[2]][1]
  ts = round(as.numeric(Sys.time())*1000)
  if (is.character(private_key) && is.character(public_key)){
    to_hash = sprintf("%s%s%s",
                      ts,
                      private_key,
                      public_key)
  }else{
    warning("API keys must be string.")
    return(-1)
  }

  params = list(
    ts=ts,
    hash=digest::digest(to_hash, "md5", FALSE),
    apikey=public_key
  )

  if (!is.null(id)){
    params$id = id
  }
  if (!is.null(offset)){
    params$offset = offset
  }
  if (!is.null(limit)){
    params$limit = limit
  }
  # Call API
  response = httr::GET("https://gateway.marvel.com:443/v1/public/comics",
                 query=params)

  if (response$status_code!=200){
    warning("Unsuccessful API call. Check your input.")
    return(-1)
  }

  # data container
  data = httr::content(response)$data
  results = data$results
  num_objects = length(results)

  # If there is no matching characters
  if (num_objects == 0){
    warning("There is no matching comics")
    return(0)
  }

  # if the user requests information of more than one comics, return the brief result
  # brief result: id, title, issue number
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*3), ncol = 3)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      title = info$title
      issue_number = info$issueNumber
      mat[i,] = cbind(id, title, issue_number)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Title", "Issue Number")
    return(df)
  }

  # if the user only requests information of one comics
  results = results[[1]]
  id = results$id
  title = results$title
  issue_number = results$issueNumber
  description = results$description
  creators_available = results$creators$available
  characters_available = results$characters$available
  stories_available = results$stories$available
  events_available = results$events$available


  # get character items' names
  character_items = results$characters$items
  character_num = length(character_items)
  # if character$items is an empty list
  if (character_num == 0){
    character_names = "NA"
  }else{
    character_names = character_items[[1]]$name
    if (character_num>1){
      for (i in 2:character_num){
        character_names = paste(character_names, character_items[[i]]$name, sep=", ")
      }
    }
  }


  df = data.frame(rbind(id, title, issue_number, description, creators_available,
                        characters_available, character_names, stories_available, events_available))
  colnames(df) = " "
  rownames(df) = c("ID", "Title", "Issue Number", "Number of Creators", "Number of Characters",
                   "Character Names", "Number of Stories", "Number of Events")
  return(df)

}
