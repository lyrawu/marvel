#' Get information on Marvel stories.
#'
#' The get_stories API gets information on Marvel stories
#' Optionally, users can specify the ID of a story.
#' Users can also specify the offset and limit of the response.
#' If there is only one returned story, get_stories API will return a dataframe containing the
#' detailed information of the story. If there are several returned stories, this API will only
#' return brief information on each story.
#'
#' @param id unique creator id. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the event(s)' information
#' @examples
#' get_stories()
#' get_stories(id = 12)
#' get_stories(offset = 1000, limit = 3)
#' @export

get_stories = function(id = NULL, offset = NULL, limit = NULL){
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
    warning("API keys must be strings.")
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
  response = httr::GET("https://gateway.marvel.com:443/v1/public/stories",
                 query=params)

  if (response$status_code!=200){
    warning("Unsuccessful API call. Check your input.")
    return(-1)
  }

  # data container
  data = httr::content(response)$data
  results = data$results
  num_objects = length(results)

  # If there is no matching creators
  if (num_objects == 0){
    warning("There is no matching comics")
    return(0)
  }

  # if the user requests information of more than one series, return the brief result
  # brief result: id and title
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*2), ncol = 2)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      title = info$title

      mat[i,] = cbind(id, title)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Title")
    return(df)
  }

  # if the user only requests information of one series
  results = results[[1]]
  id = results$id
  title = results$title
  description = results$description

  type = results$type


  creators_available = results$creators$available
  characters_available = results$characters$available
  series_available = results$series$available
  comics_available = results$comics$available
  events_available = results$events$available
  original_issue = ifelse(is.null(results$originalIssue), "NA", results$originalIssue$name)



  # get creators items' names
  creators_items = results$creators$items
  creators_num = length(creators_items)
  # if creators$items is an empty list
  if (creators_num == 0){
    creators_names = "NA"
  }else{
    creators_names = creators_items[[1]]$name
    if (creators_num>1){
      for (i in 2:creators_num){
        creators_names = paste(creators_names, creators_items[[i]]$name, sep=", ")
      }
    }
  }

  # get character items' names
  characters_items = results$characters$items
  characters_num = length(characters_items)
  # if character$items is an empty list
  if (characters_num == 0){
    characters_names = "NA"
  }else{
    characters_names = characters_items[[1]]$name
    if (characters_num>1){
      for (i in 2:characters_num){
        characters_names = paste(characters_names, characters_items[[i]]$name, sep=", ")
      }
    }
  }

  # get series items' names
  series_items = results$series$items
  series_num = length(series_items)
  # if series$items is an empty list
  if (series_num == 0){
    series_names = "NA"
  }else{
    series_names = series_items[[1]]$name
    if (series_num>1){
      for (i in 2:series_num){
        series_names = paste(series_names, series_items[[i]]$name, sep=", ")
      }
    }
  }

  # get comics items' names
  comics_items = results$comics$items
  comics_num = length(comics_items)
  # if comics$items is an empty list
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

  # get events items' names
  events_items = results$events$items
  events_num = length(events_items)
  # if events$items is an empty list
  if (events_num == 0){
    events_names = "NA"
  }else{
    events_names = events_items[[1]]$name
    if (events_num>1){
      for (i in 2:events_num){
        events_names = paste(events_names, events_items[[i]]$name, sep=", ")
      }
    }
  }

  df = data.frame(rbind(id, title, description, type, creators_available, creators_names,
                        characters_available, characters_names, series_available, series_names,
                        comics_available, comics_names, events_available, events_names, original_issue))
  colnames(df) = " "


  return(df)

}
