pal_to_img <- function(colors=c("#626469",
                                "#865c43",
                                "#c7a27c",
                                "#8d8274",
                                "#434245")
                       ){
  base_url <- "https://artsexperiments.withgoogle.com/artpalette/colors/"
  url <- paste0(base_url, paste(gsub("#","",colors),collapse="-"))
  tmp <- rvest::read_html(url)
  tmp |> rvest::html_nodes(".result-item")

}
