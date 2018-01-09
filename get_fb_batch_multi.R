#' Submit batch requests to Facebook API
#'
#' @param token
#' Your API token
#'
#' @param relative_urls
#' A list of relative URLs to be submitted within each GET request submitted to the Facebook API.
#'
#' @return
#' Returns a list containing both 'data' and 'values'. Data is the full data from the API response. 'Values' is the values property from the response, which can contain either one or several values.
#'
#' @export
#'
#' @examples
#' get_fb_batch(token = "ple", relative_urls = "someurl")
get_fb_batch_multi <- function(token, relative_urls) {
  library(httr)
  
  batchUrl <- ""
  # Build batch query list
  for ( i in seq_along(relative_urls) ) {
    relurl <- relative_urls[i]
    batchUrl <- paste0(batchUrl, "{'method': 'GET', 'relative_url': '", relurl, "'}")
    if ( i < length(relative_urls)) {
      batchUrl <- paste0(batchUrl, ",")
    }
  }
  batchUrl <- paste0("[", batchUrl, "]")
  
  url <- "https://graph.facebook.com"
  
  postRequest <- POST( url = url, query = list(batch=batchUrl, access_token=token))
  response <-content(postRequest, as="text")
  jsonData <- jsonlite::fromJSON(response)
  
  returnData <- data.frame(id = str_split(relative_urls[1], "\\/")[[1]][1])
  for ( i in 1:length(jsonData$body) ) {
    data <- fromJSON(jsonData$body[i])$data
    values <- fromJSON(jsonData$body[i])$data$values
    merged_data <- bind_cols(data, values[[1]]$value) %>% 
      separate(col = id,
               into = c("id", "insights"),
               sep= ("\\/insights")) %>% 
      select(-c(name, period, values, title, description, insights)) %>% 
      select(id, everything())
    returnData <- left_join(returnData, merged_data)
    
  }
  return(returnData)
}
