#' Google StreetArt Search
#'
#' Search the
#' \href{https://artsandculture.google.com/color?project=street-art}{
#' Street Art curation project} by \link{https://artsandculture.google.com/}{
#' Google Arts & Culture}.
#' @param color Color to search the database by.
#' @param max_n Max number of images to return.
#' @param get_metadata Whether to collect metadata on each image as well
#' (e.g. "Creator","Title","Location","Date").
#' @returns dat.table of search results.
#'
#' @export
#' @import data.table
#' @examples
#' d <- google_streetart_search(color="#9F55CF")
google_streetart_search <- function(color="#1BE5E1",
                                    max_n=NULL,
                                    get_metadata=FALSE){
  #### Get color options ####
  # tmp <- rvest::read_html(
  #   "https://artsandculture.google.com/color?project=street-art&col=RGB_518077")
  # tmp |> rvest::html_nodes(".DNS5qc")

  #### Search images ####
  tmp <- paste0(
    "https://artsandculture.google.com/color?project=street-art&col=RGB_",
    color) |> rvest::read_html()
  # "e0WtYb HpzMff PJLMUc"
  entries <- tmp |>
    rvest::html_nodes(".e0WtYb.HpzMff")
  d <- data.table::as.data.table(
    list(
      color = color,
      url = entries |>
        rvest::html_attr("data-bgsrc") |>
        gsub(pattern="^//",replacement="https://"),
      href = entries |>
        rvest::html_attr("href") |>
        gsub(pattern="^/",replacement="https://artsandculture.google.com/")
    )
  )
  #### Limit number of metadata queries ####
  if(!is.null(max_n)) d <- d[seq(max_n),]
  #### Get metadata ####
  if(isTRUE(get_metadata)){
    meta <- lapply(stats::setNames(d$href,d$href), function(x){
      cat("Processing: ",x,"\n")
      tmp <- rvest::read_html(x)
      nodes <- tmp |> rvest::html_nodes(".XD0Pkb")
      m <- data.table::as.data.table(
        list(
          key = nodes |> rvest::html_nodes(".PUhAff") |>
            rvest::html_text() |>
            gsub(pattern=" +",replacement=""),
          value = nodes |>
            rvest::html_text()
        )
      )[,value:=gsub(paste0(key," "),"",value), by=key][,key:=gsub(":","",key)]
      m <- rbind(
        m,
        data.table::as.data.table(
          list(
            key="Description",
            value=tmp |>
              rvest::html_nodes(".WDSAyb.QwmCXd") |>
              rvest::html_text()
          )
        )
      )
      m
    }) |> data.table::rbindlist(fill = TRUE,
                                use.names = TRUE,
                                idcol = "href") |>
      data.table::dcast.data.table(fun.aggregate = unique,
                                   formula = "href~key",
                                   value.var = "value",
                                   fill = NA)
    d <- d[meta, on="href"]
  }
  return(d)
}

