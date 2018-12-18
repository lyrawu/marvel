#' Get information on Marvel characters
#'
#' The get_characters function gets information on Marvel characters
#' Optionally, users can specify the name or the id (not both) of a character.
#' Users can also specify the offset and limit of the response
#' If there is only one requested character, get_characters API will return a dataframe containing the
#' detailed information of the character. If users request several characters, this API will only
#' return the brief information on each character.
#'
#' @param id unique character id. Not necessary if name already specified. Optional.
#' @param name character name. Not necessary if id already specified. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the character(s)' information
#' @examples
#' get_characters()
#' get_characters(id = 1011334)
#' get_characters(offset = 50, limit = 3)
#' @export


get_characters = function(id = NULL, name = NULL, offset = NULL, limit = NULL){
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
  if (!is.null(name)){
    params$name = name
  }
  if (!is.null(offset)){
    params$offset = offset
  }
  if (!is.null(limit)){
    params$limit = limit
  }

  # Call API
  response = httr::GET("https://gateway.marvel.com:443/v1/public/characters",
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
    warning("There is no matching characters.")
    return(0)
  }

  # if the user requests information of more than one character, return the brief result
  # brief result: id, name, comics available
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*3), ncol = 3)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      name = info$name
      comics_available = info$comics$available
      mat[i,] = cbind(id, name, comics_available)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Name", "Comics")
    return(df)
  }

  # if the user only requests information of one character
  results = results[[1]]
  id = results$id
  name = results$name
  description = results$description
  comics_available = results$comics$available
  series_available = results$series$available
  stories_available = results$stories$available
  events_available = results$events$available

  # get comic items' names
  comics_items = results$comics$items
  comics_num = length(comics_items)
  if (comics_num == 0){
    comics_names = "NA"
  }else{
    comics_names = comics_items[[1]]$name
    if (comics_num>1){
      for (i in 2:comics_num){
        comics_names = paste(comics_names, comics_items[[i]]$name, sep=", ")
      }
    }
  }


  df = data.frame(rbind(id, name, description, comics_available, comics_names,
                        series_available, stories_available, events_available))
  colnames(df) = " "
  rownames(df) = c("ID", "Name", "Description", "Number of Comics", "Comics Names",
                   "Number of Series", "Number of Stories", "Number of Events")
  return(df)

}
