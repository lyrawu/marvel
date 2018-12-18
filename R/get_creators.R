#' Get information on Marvel creators.
#'
#' The get_creators API returns information on Marvel creators.
#' Optionally, users can specify the ID of a creator.
#' Users can also specify the offset and limit of the response.
#' If there is only one returned creator, get_comics API will return a dataframe containing the
#' detailed information of the creator If there are several returned creators, this API will only
#' return brief information on each comics.
#'
#' @param id unique creator id. Optional.
#' @param offset The requested number of skipped results of the call. Optional.
#' @param limit The requested results limit. Optional.
#' @return a dataframe containing the creator(s)' information
#' @examples
#' get_creators()
#' get_creators(id = 3188)
#' get_creators(offset = 50, limit = 3)
#' @export

get_creators = function(id = NULL, offset = NULL, limit = NULL){
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
  response = httr::GET("https://gateway.marvel.com:443/v1/public/creators",
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
  # brief result: id, full name, comics available
  if (num_objects > 1){
    mat = matrix(rep(0,num_objects*3), ncol = 3)
    for (i in 1:num_objects){
      info = results[[i]]
      id = info$id
      full_name = info$fullName
      if (full_name == ""){
        full_name = "NA"
      }
      comics_available = info$comics$available
      mat[i,] = cbind(id, full_name, comics_available)
    }
    df = data.frame(mat)
    colnames(df) = c("ID", "Full Name", "Number of Comics")
    return(df)
  }

  # if the user only requests information of one creator
  results = results[[1]]
  id = results$id
  full_name = results$fullName
  if (full_name == ""){
    full_name = "NA"
  }
  comics_available = results$comics$available
  series_available = results$series$available
  stories_available = results$stories$available
  events_available = results$events$available


  # get comics items' names
  comics_items = results$comics$items
  comics_num = length(comics_items)
  # if character$items is an empty list
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
  # if character$items is an empty list
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



  df = data.frame(rbind(id, full_name, comics_available, comics_names, series_available,
                        stories_available, events_available, events_names))
  colnames(df) = " "
  rownames(df) = c("ID", "Full Name", "Number of Comics", "Comics Names", "Number of Series",
                   "Number of Stories", "Number of Events", "Events Names")
  return(df)

}
