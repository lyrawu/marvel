#' Get information on Marvel series.
#'
#' The get_series API gets information on Marvel series.
#' Optionally, users can specify the ID of an event.
#' Users can also specify the offset and limit of the response.
#' If there is only one returned event, get_series API will return a dataframe containing the
#' detailed information of the event. If there are several returned series, this API will only
#' return brief information on each series.
#'
#' @param id unique creator id. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the event(s)' information
#' @examples
#' get_series()
#' get_series(id = 1234)
#' get_series(offset = 50, limit = 3)
#' @export


get_series = function(id = NULL, offset = NULL, limit = NULL){
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
  response = httr::GET("https://gateway.marvel.com:443/v1/public/series",
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
  # brief result: id, full name, start and end year
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*3), ncol = 3)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      title = info$title

      start = ifelse(is.null(info$startYear), "NA", info$startYear)
      end = ifelse(is.null(info$endYear), "NA", info$endYear)
      start_end_date = paste(start, end, sep = " - ")

      mat[i,] = cbind(id, title, start_end_date)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Title", "Start and End Date")
    return(df)
  }

  # if the user only requests information of one series
  results = results[[1]]
  id = results$id
  title = results$title
  description = results$description

  if (is.null(results$startYear)){
    start = "NA"
  }else{
    start = results$startYear
  }

  if (is.null(results$endYear)){
    end = "NA"
  }else{
    end = results$endYear
  }

  start_end_date = paste(start, end, sep = " - ")

  rating = results$rating
  if (rating==""){rating = "NA"}
  type = results$type
  if (type==""){type = "NA"}

  creators_available = results$creators$available
  characters_available = results$characters$available
  stories_available = results$stories$available
  comics_available = results$comics$available
  events_available = results$events$available

  if (is.null(results$'next')){
    next_event = NA
  }else{
    next_event = results$'next'$name
  }

  if (is.null(results$previous)){
    previous_event = NA
  }else{
    previous_event = results$previous$name
  }


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

  # get story items' names
  stories_items = results$stories$items
  stories_num = length(stories_items)
  # if character$items is an empty list
  if (stories_num == 0){
    stories_names = "NA"
  }else{
    stories_names = stories_items[[1]]$name
    if (stories_num>1){
      for (i in 2:stories_num){
        stories_names = paste(stories_names, stories_items[[i]]$name, sep=", ")
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

  df = data.frame(rbind(id, title, description, start_end_date, rating, type, creators_available, creators_names,
                        characters_available, characters_names, stories_available, stories_names,
                        comics_available, comics_names, events_available, events_names,
                        previous_event, next_event))
  colnames(df) = " "


  return(df)

}
