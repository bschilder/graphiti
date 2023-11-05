streetart_api_search <- function(rapidapi_key,){

  requireNamespace("httr")
  headers <- c(
    `X-RapidAPI-Host` = "street-art.p.rapidapi.com",
    `X-RapidAPI-Key` = rapidapi_key
  )
  url <- "https://street-art.p.rapidapi.com/city/Los"
  response <- httr::GET(url, add_headers(.headers=headers))
  content <- httr::content(response)
  # $messages
  # [1] "The API is unreachable, please contact the API provider"
  # $info
  # [1] "Your Client (working) ---> Gateway (working) ---> API (not working)"

}
