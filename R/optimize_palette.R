#' @title optimize raw palette obtained from create_palette
#' @description taken a raw palette created from a jpeg image, optimizes it given the type of variable to be plotted
#' @param rgb_raw_palette numeric matrix of RGB from a call to rgb on hex codes
#' @param number_of_colours integer, number of desired colours in the final palette, as specified by the user when calling create_palette
#' @param type_of_variable string, default to 'categorical'. type of variable to be plotted with the building palette
#' @param effective_n_of_color integere, the actual number of colors obtained from the application of kmeans on the image. equal to number_of_colours *100
#' @param filter_on_low_brightness boolean, default to true. specifies if a filter on colours with low brigthness should be applied to enhance the palette
#' @param filter_on_high_brightness boolean, default to true. specifies if a filter on colours with high brigthness should be applied to enhance the palette
#' @param filter_on_saturation boolean, default to ture. specifies if a filter on low saturation should be applied.
#' @details palette optimization consists into four different steps:
#' - conversion to hsv scale in order to easily elavorate on colour order and properties.
#' - filter on colours with a brightness lower than the first quartile of v distribution
#' - filter on colours with a brightness higher or equal to the Tukey's outlier threshold computed on the overall v distribution
#' - subset of the palette according to the type of variable to be plotted: a spaced sample in case of categorical variables, an interpolation between two colours close to the mode of h for continous variables
#' @author Andrea Cirillo
#' @examples
#' create_palette("data/nascita_venere.jpg",number_of_colors = 20)
#' @export
optimize_palette <- function(rgb_raw_palette = NA,
                             number_of_colors =NA ,
                             type_of_variable ="categorical",
                             effective_n_of_color=NA,
                             filter_on_low_brightness=NA,
                             filter_on_high_brightness=NA,
                             filter_on_saturation = NA){

#some check on the length of colour vector

# preparing hsv palette
rgb_raw_palette %>%
  t() %>%
  rgb2hsv() %>%
  t() %>%
  data.frame() %>%
  mutate(id = row.names(.)) -> hsv_raw_palette

#preparing vector of rgb text vector and their id

rgb(rgb_raw_palette) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(id = as.character(seq(1:effective_n_of_color))) -> rgb_text_vector

colnames(rgb_text_vector) <-  c("hex_code", "id")

# merging all the three palette to obtain a complete one to be sorted.
rgb_raw_palette %>%
  data.frame() %>%
  mutate(id = row.names(.)) %>%
  left_join(hsv_raw_palette) %>%
  left_join(rgb_text_vector) %>%
  arrange(h,s,v) -> sorted_raw_palette

#remove black and white

sorted_raw_palette %>%
  filter( v != 0) %>%
  filter(s != 0)-> sorted_raw_palette

# if no override was requested for the default filter on low brightness we remove the
# lower tale of brightness distribution
brightness_stats <- boxplot.stats(sorted_raw_palette$v)
saturation_stats <- boxplot.stats(sorted_raw_palette$s)
if (filter_on_low_brightness == TRUE){

  first_quartile_v <- round(brightness_stats$stats[2],4)

  sorted_raw_palette %>%
    filter(v > first_quartile_v) -> sorted_raw_palette
  effective_n_of_color <- nrow(sorted_raw_palette)# this number can never be lower than number_of colours

}

if (filter_on_high_brightness == TRUE){

  outlier_threshold_v <- brightness_stats$stats[5]

  sorted_raw_palette %>%
    filter(v < outlier_threshold_v) -> sorted_raw_palette
  effective_n_of_color <- nrow(sorted_raw_palette)# this number can never be lower than number_of colours

}


if (filter_on_saturation == TRUE){

  first_quartile_s <- round(saturation_stats$stats[2],4)

  sorted_raw_palette %>%
    filter(s > first_quartile_s) -> sorted_raw_palette
  effective_n_of_color <- nrow(sorted_raw_palette)# this number can never be lower than number_of colours

}

#conditional statement <- if categorical I sample distant colours, else I create a vector of n_of_colours
## around the mode of the distribution

if(type_of_variable == "categorical"){

 # spaced_indexes <- seq(from=1, to =effective_n_of_color,by = effective_n_of_color/number_of_colors)
 result <- data.frame()
  for (i in 1:1000){
    spaced_indexes_rand <- sort(sample(seq(from=1, to =effective_n_of_color,by = 1),size = number_of_colors))
    sorted_raw_palette[spaced_indexes_rand,] %>%
      select(h) -> h_temp
    vector <- paste(spaced_indexes_rand,collapse = ",")
    delta <- median(diff(h_temp$h))
    evaluate <- cbind(vector = vector,delta = delta)
    rm(h_temp)
    result <- rbind(result,evaluate)

  }
 result %>%
   arrange(desc(delta)) %>%
   select(vector) %>%
    head(1) %>%
   unlist() %>%
   as.character() %>%
   strsplit(split = ",") %>%
   as.vector() %>%
   unlist() %>%
   as.numeric()-> selected_indexes

   final_palette <- sorted_raw_palette$hex_code[selected_indexes]
  show_col(final_palette)

  }else if ( type_of_variable == "continous"){

    #we compute the mode of the raw palette and add it back as an attribute
    sorted_raw_palette %>%
      select(id,h) %>%
      mutate(root_hue = round(h,2)) %>%
      group_by(root_hue) %>%
      count() %>%
      arrange(desc(n)) -> root_hues_table
    mode_hue <- root_hues_table[1,1]

    descriptive_v <- summary(round(sorted_raw_palette$v,4))
    third_quartile_v <- descriptive_v[5]

    sorted_raw_palette %>%
      mutate(mode_hue = as.numeric(rep(mode_hue,nrow(.))),
             quartile_v = as.numeric(rep(third_quartile_v,nrow(.))),
             distance_fom_quartile = abs(v - quartile_v),
             distance_from_mode = abs(h - mode_hue)) %>%# we compute distance from the mode
      arrange(distance_from_mode, distance_fom_quartile) %>% # sorting in order to get a colour near to the mode and bright enough
      head(n = 2) %>%  # we select the shortest distance available and the immediately subsequent record (colour)
      select(hex_code) %>%
      pull() %>%
      as.vector() -> hex_codes
    # we interpolate number_of_colours color between the nearest to the mode and the subsequent
     gradient_builder <- colorRampPalette(colors = hex_codes)
     final_palette <- gradient_builder(number_of_colors)

}else{
  stop("you must specify a valid token for type_of_variable argument")
}
return(final_palette)
}
