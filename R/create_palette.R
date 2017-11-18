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
create_palette <- function(image_path = NA, number_of_colors = 40, type_of_variable = NA,filter_on_low_brightness= TRUE,filter_on_high_brightness=TRUE){

  if (is.na(image_path)){stop("you must provide a jpg image to create your palette from")}
  painting     <- readJPEG(image_path)
  dimension    <- dim(painting)
  effective_n_of_color <- number_of_colors*100 #we increase granularity to subsequently optimize the palette
  painting_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(painting[,,1]),
    G = as.vector(painting[,,2]),
    B = as.vector(painting[,,3])
  )
  k_means        <- kmeans(painting_rgb[,c("R","G","B")], centers = effective_n_of_color, iter.max = 30)
  rgb_raw_palette <- k_means$centers
  # call to optimize palette
  final_palette <- optimize_palette(rgb_raw_palette,
                                    number_of_colors,
                                    type_of_variable = type_of_variable,
                                    effective_n_of_color,
                                    filter_on_low_brightness = filter_on_low_brightness,
                                    filter_on_high_brightness = filter_on_high_brightness)
  show_col(final_palette)
  return(final_palette)
}
