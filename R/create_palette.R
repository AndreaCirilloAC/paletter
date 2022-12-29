#' @title make a palette from your image
#' @description processes a custom jpeg image producing a palett with a user-defined number of colours
#' @param image_path string,default to NA. relative path to the custom image, including .jpeg extension
#' @param number_of_colours integer, default to 40. number of desired colours in the final palette, as specified by the user when calling create_palette
#' @param type_of_variable string, default to 'categorical'. type of variable to be plotted with the building palette
#' @param filter_on_low_brightness boolean, default to true. specifies if a filter on colours with low brigthness should be applied to enhance the palette
#' @param filter_on_high_brightness boolean, default to true. specifies if a filter on colours with high brigthness should be applied to enhance the palette
#' @param optimize_palette boolean, default to true. specifies if palette optimization algorithm should be applied or not
#' @param filter_on_saturation boolean, default to ture. specifies if a filter on low saturation should be applied.
#' @importFrom stats kmeans median
#' @importFrom scales show_col
#' @importFrom jpeg readJPEG
#' @importFrom utils head
#' @details palette creation and optimization ist started drawing a raw palette of rgb colours from the jpeg image provided.
#' The palette is then optimized applying the following four steps:
#' - conversion to hsv scale in order to easily elavorate on colour order and properties.
#' - filter on colours with a brightness lower than the first quartile of v distribution
#' - filter on colours with a brightness higher or equal to the Tukey's outlier threshold computed on the overall v distribution
#' - subset of the palette according to the type of variable to be plotted: a spaced sample in case of categorical variables, an interpolation between two colours close to the mode of h for continuous variables
#' @author Andrea Cirillo
#' @examples
#' create_palette("data/nascita_venere.jpg",number_of_colors = 20, type_of_variable = "categorical")
#' @export
create_palette <- function(image_path = NA,
                           number_of_colors = 40,
                           type_of_variable = NA,
                           filter_on_low_brightness= TRUE,
                           filter_on_high_brightness= TRUE,
                           optimize_palette = TRUE,
                           filter_on_saturation = TRUE){

  if (is.na(image_path)){stop("you must provide a jpg image to create your palette from")}
  if (is.na(type_of_variable)){stop("you must specify a valid type_of_variable argument to create the palette")}
  message("decomposing image into RGB...")
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
  if (optimize_palette == TRUE){
  message("applying kmeans to the image...")
  k_means        <- kmeans(painting_rgb[,c("R","G","B")], centers = effective_n_of_color, iter.max = 30)
  rgb_raw_palette <- k_means$centers
  # call to optimize palette
  message("optimising palette...")
  final_palette <- optimize_palette(rgb_raw_palette,
                                    number_of_colors,
                                    type_of_variable = type_of_variable,
                                    effective_n_of_color,
                                    filter_on_low_brightness = filter_on_low_brightness,
                                    filter_on_high_brightness = filter_on_high_brightness,
                                    filter_on_saturation = filter_on_saturation)
  }else{
    message("applying kmeans to the image...")
    k_means         <- kmeans(painting_rgb[,c("R","G","B")], centers = number_of_colors, iter.max = 30)
    rgb_raw_palette <- k_means$centers
    final_palette   <- rgb(k_means$centers)
  }
  show_col(final_palette)
  return(final_palette)
}
