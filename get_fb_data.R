#' Get Facebook Insights data for a page or post
#'
#' @param metric
#' The metric you'd like to pull. Refer to https://developers.facebook.com/docs/graph-api/reference/v2.7/insights for details of available metrics.
#' @param pageOrPost
#' A character object containing the name of the page or post (e.g. "YourBrandUK" ) for which you'd like to pull the given metric.
#' @param token
#' (string), a user access token, obtained from the facebook graph API explorer at https://developers.facebook.com/tools/explorer.
#' @param daterange
#' A character vector containing 2 date strings in format YYYY-mm-dd to specify the date from and to.
#' @param period
#' The period for which the metric should be returned. Defaults to "day". Certain metrics from the Facebook API have differing periodicity. Again, refer to https://developers.facebook.com/docs/graph-api/reference/v2.7/insights for further details.
#'
#' @return
#' Returns a data frame of the results from the Facebook API explorer
#' @export
#'
#' @examples
#' get_fb_insights(metric = "page_engaged_users", pageOrPost = "YourBrandUSA", daterange = c("2016-08-01", "2016-08-14"), token = "asfADFSj43554jkklj")
#' Extremely useful reference: https://www.sammyk.me/optimizing-request-queries-to-the-facebook-graph-api
get_fb_insights <- function(metric = "page_consumptions_by_consumption_type",
                        pageOrPost = "YourBrand",
                        token,
                        daterange = c(Sys.Date()-30, Sys.Date()-1),
                        period = "day") {
  if ( !require(httr) ) install.packages("httr")
  if ( !require(jsonlite) ) install.packages("jsonlite")
  if ( !require(lubridate) ) install.packages("lubridate")
  if ( !require(reshape2) ) install.packages("reshape2")
  if ( !require(dplyr) ) install.packages("dplyr")

  # Set the URL for get request, then get & parse the json response
  apiUrl <- "https://graph.facebook.com/v2.7/"
  insightsData <- NULL
  insightFragment <- ifelse(grepl("video_", metric), "/video_insights/", "/insights/")
  pageOrPost <- ifelse(grepl("video_", metric), strsplit(pageOrPost, "_")[[1]][2], pageOrPost)
  url <- paste0(apiUrl,pageOrPost,insightFragment,metric,"?since=",daterange[1],"&until=",daterange[2],"&period=",period,"&access_token=",token)
  getRequest <- GET( url = url)
  response <-content(getRequest, as="text")
  jsonData <- jsonlite::fromJSON(response)

  # Control flow to handle single / multiple values returned
    if ( length(jsonData$data) == 0  ) {
      message("No results were found for this metric / time period.")
      return(insightsData)
    } else if ( !is.null(ncol(as.data.frame(as.data.frame(jsonData$data$values))$value))  ) { # multiple values found
      message("metric returns more than 1 value")

      # Get value columns from json data, tidy and bind with date data
      insightsData <- as.data.frame(as.data.frame(jsonData$data$values))$value
      insightsData$node <- pageOrPost
      if ( ! is.null(as.data.frame(jsonData$data$values)$end_time)) {
        insightsData$end_time <- as.data.frame(jsonData$data$values)$end_time
      }

    } else {
      message("metric returns 1 value")
      insightsData <- as.data.frame(jsonData$data$values)
      insightsData$node <- pageOrPost

    }
  if ( !is.null(insightsData$end_time) ) {
    insightsData <- insightsData %>% select(end_time, node, everything())
  } else {
    insightsData <- insightsData %>% select(node, everything())
  }
  return(insightsData)
}



#' Get a dataframe of postIDs & post times/dates from a given Facebook page.
#'
#' @param page
#' A character object containing the name of the page for which you'd like to pull the given metric.
#' @param token
#' (string), a user access token, obtained from the facebook graph API explorer at https://developers.facebook.com/tools/explorer.
#' @param daterange
#' A character vector containing 2 date strings in format YYYY-mm-dd to specify the date from and to.
#'
#' @return
#' Returns a dataframe containing the post IDs, posted dates, posted times and post message (if available).
#' @export
#'
#' @examples
#' UKposts <- get_fb_posts(page = "YourBrandUK", daterange = c("2016-08-01", "2016-08-14"), token = "asfADFSj43554jkklj")
get_fb_posts <- function(page = "YourBrand",
                            token,
                            daterange = c(Sys.Date()-30, Sys.Date()-1)) {
  if ( !require(httr) ) install.packages("httr")
  if ( !require(jsonlite) ) install.packages("jsonlite")
  if ( !require(lubridate) ) install.packages("lubridate")
  if ( !require(reshape2) ) install.packages("reshape2")
  if ( !require(dplyr) ) install.packages("dplyr")

  # Set the URL for get request, then get & parse the json response
  apiUrl <- "https://graph.facebook.com/v2.7/"
  postData <- NULL
  url <- paste0(apiUrl,page,"/posts/?since=",daterange[1],"&until=",daterange[2],"&access_token=",token)
  getRequest <- GET( url = url)
  response <-content(getRequest, as="text")
  jsonData <- jsonlite::fromJSON(response)

  # Control flow to handle single / multiple values returned
  if ( length(jsonData$data) == 0  ) {
    message("No results were found for this metric / time period.")
    return(postData)
  } else {
    message(paste("Found post data for", page))
    postData <- as.data.frame(jsonData$data)
  }

  if ( sum(grepl("story", names(postData))) >0 ) {
    postData <- select(postData, -`story`)
  }
  if ( sum(grepl("message", names(postData))) == 0 ) {
    postData$message <- ""
  }
  postData$page <- page
  postData <- postData %>% select(id, page, created_time, everything())
  return(postData)
}
