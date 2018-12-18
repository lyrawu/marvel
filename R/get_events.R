#' Get information on Marvel events.
#'
#' The get_events API returns information on Marvel events.
#' Optionally, users can specify the ID of an event or the name of an event.
#' Users can also specify the offset and limit of the response.
#' If there is only one returned event, get_comics API will return a dataframe containing the
#' detailed information of the event. If there are several returned events, this API will only
#' return brief information on each event.
#'
#' @param id unique creator id. Optional.
#' @param name title of an event. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the event(s)' information
#' @examples
#' get_events()
#' get_events(id = 321)
#' get_events(name = "Spider-Verse")
#' get_events(offset = 50, limit = 3)
#' @export


get_events = function(id = NULL, name = NULL, offset = NULL, limit = NULL){
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
  response = httr::GET("https://gateway.marvel.com:443/v1/public/events",
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

  # if the user requests information of more than one comics, return the brief result
  # brief result: id, full name, start and end date
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*3), ncol = 3)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      title = info$title

      if (is.null(info$start)){
        start = "NA"
      }else{
        start = strsplit(info$start, " ")[[1]][1]
      }

      if (is.null(info$end)){
        end = "NA"
      }else{
        end = strsplit(info$end, " ")[[1]][1]
      }

      start_end_date = paste(start, end, sep = " - ")
      mat[i,] = cbind(id, title, start_end_date)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Title", "Start and End Date")
    return(df)
  }

  # if the user only requests information of one creator
  results = results[[1]]
  id = results$id
  title = results$title
  description = results$description

  if (is.null(results$start)){
    start = "NA"
  }else{
    start = strsplit(results$start, " ")[[1]][1]
  }

  if (is.null(results$end)){
    end = "NA"
  }else{
    end = strsplit(results$end, " ")[[1]][1]
  }

  start_end_date = paste(start, end, sep = " - ")

  creators_available = results$creators$available
  characters_available = results$characters$available
  stories_available = results$stories$available
  comics_available = results$comics$available
  series_available = results$series$available

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


  df = data.frame(rbind(id, title, description, start_end_date, creators_available, characters_available,
                        characters_names, stories_available, stories_names, comics_available,
                        series_available, previous_event, next_event))
  colnames(df) = " "
  rownames(df) = c("ID", "Title", "Description", "Start and End Date", "Number of Creators",
                   "Number of Characters", "Character Names",
                   "Number of Stories", "Story Names", "Number of Comics",
                   "Number of Series", "Previous Event", "Next Event")
  return(df)

}
