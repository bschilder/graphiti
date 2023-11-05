google_arts_search <- function(){
  library(httr)
  library(stringr)

  AXIOS_OPTIONS <- list(
    base_url = "https://artsandculture.google.com",
    headers = c(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36"
    ),
    query = list(
      hl = "en"
    )
  )

  get_results_from_category <- function(category_content) {
    artists_pattern <- 'cobject","(?<artist>[^"]+)","(?<works>[^ ]+) \\w+","(?<thumbnail>[^"]+)","(?<link>[^"]+)'

    matches <- str_match_all(category_content, artists_pattern)[[1]]

    results <- lapply(matches, function(match) {
      list(
        artist = match[["artist"]],
        works = match[["works"]],
        thumbnail = paste("https:", match[["thumbnail"]], sep = ""),
        link = paste(AXIOS_OPTIONS$base_url, substr(match[["link"]], 2, nchar(match[["link"]]) - 1), sep = "")
      )
    })

    return(results)
  }

  get_artists_info <- function() {
    response <- GET(paste(AXIOS_OPTIONS$base_url, "/category/artist", sep = ""),
                    add_headers(.headers = AXIOS_OPTIONS$headers),
                    query = AXIOS_OPTIONS$query)
    data <- content(response, as = "text")

    results <- list()

    popular_category_pattern <- '"PopularAssets:(?<content>.+?)\\["stella\\.pr'
    popular_matches <- str_match_all(data, popular_category_pattern)[[1]]
    results$popular <- get_results_from_category(popular_matches[[1, "content"]])

    az_category_pattern <- '"(?<letter>[^"])",\\["stella\\.pr","(?<content>.+?)[\\w"||\\d]\\]{2,3},\\['
    az_matches <- str_match_all(data, az_category_pattern)[[1]]
    for (i in seq_along(az_matches)) {
      letter <- az_matches[[i, "letter"]]
      content <- az_matches[[i, "content"]]
      results[[letter]] <- get_results_from_category(content)
    }

    time_category_pattern <- '\\[{1,2}"(?<time>[^"]{3,8})","?\\w{4,7}.+?\\["stella\\.pr","DatedAssets(?<content>.+?)"?\\d{3,5}"\\]'
    time_matches <- str_match_all(data, time_category_pattern)[[1]]
    for (i in seq_along(time_matches)) {
      time <- time_matches[[i, "time"]]
      content <- time_matches[[i, "content"]]
      results[[time]] <- get_results_from_category(content)
    }

    return(results)
  }

  result <- get_artists_info()
  print(result)
}
