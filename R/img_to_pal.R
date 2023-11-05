#' Image to palette
#'
#' Convert an image to a color palette.
#' @inheritParams imgpalr::image_pal
#'
#' @export
#' @import imgpalr
#' @examples
#' file <- system.file("blue-yellow.jpg", package = "imgpalr")
#' pal <- img_to_pal(file)
img_to_pal <- function(file){
  pal <- imgpalr::image_pal(file = file)
  return(pal)
}
