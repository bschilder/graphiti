google_photos_search <- function(){
  requireNamespace("googleAuthR")

  "https://photos.google.com/share/AF1QipPNr0IA_Gl-z5G9M85VZvvHFDnTHMq3RCX72A5tLKrVgpM2AKPI0h81aulsgZjm4A/photo/AF1QipMpwhnNn4mP1RnzVCLbLiHxVF_gT6AAi3Ru-_g?key=T1ptVnlheXNXNExBVG9fSkJXZElDbHloWHhRZE9R"


  k <- gargle::token_fetch(token = googleAuthR::gar_token())
  authorization = paste('Bearer ', k$credentials$access_token)
  # "https://photos.app.goo.gl/Mj2vTo6tF48J5Qpw6"
  getmedia <-
    httr::POST(
      glue::glue("https://photoslibrary.googleapis.com/v1/mediaItems:search"),
         httr::add_headers(
           'Authorization' = "T1ptVnlheXNXNExBVG9fSkJXZElDbHloWHhRZE9R",
           'Accept'  = 'application/json'),
         body = list("albumId" = "Mj2vTo6tF48J5Qpw6",
                     "pageSize" = 25),
         encode = "json"
    ) %>%
    httr::content(., as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    data.frame()

}

