#' @importFrom stats kmeans
#' @importFrom jpeg readJPEG
#' @importFrom scales show_col
#' @title make a palette from your image
#' @description processes a custom jpeg image producing a palett with a user-defined number of colours
#' @param image_path relative path to the custom image, including .jpeg extension
#' @param number_of_colors number of different colors desired for the resulting palette
#' @author Andrea Cirillo
#' @examples
#' create_palette("data/nascita_venere.jpg",number_of_colors = 20)
#' @export
create_palette <- function(image_path = NA, number_of_colors = 20){
  if (is.na(image_path)){stop("you must provide a jpg image to build your palette from")}
  painting     <- readJPEG(image_path)
  dimension    <- dim(painting)
  painting_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(painting[,,1]),
    G = as.vector(painting[,,2]),
    B = as.vector(painting[,,3])
  )
  k_means        <- kmeans(painting_rgb[,c("R","G","B")], centers = number_of_colors, iter.max = 30)
  colours_k      <- rgb(k_means$centers[k_means$cluster,])
  colours_vector <- unique(colours_k)
  show_col(colours_vector)
  return(colours_vector)
}