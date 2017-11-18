optimize_palette <- function(rgb_raw_palette = NA, number_of_colors =NA ,type_of_variable ="categorical",effective_n_of_color=NA,filter_on_low_brightness=NA,
                             filter_on_high_brightness=NA){

#some check on the length of colour vector

# preparing hsv palette
rgb_raw_palette %>%
  t() %>%
  rgb2hsv() %>%
  t() %>%
  data.frame() %>%
  mutate(id = row.names(.))->hsv_raw_palette

#preparing vector of rgb text vector and their id

rgb(rgb_raw_palette) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(id = as.character(seq(1:effective_n_of_color)))-> rgb_text_vector

colnames(rgb_text_vector) <-  c("hex_code", "id")

# merging all the three palette to obtain a complete one to be sorted.
rgb_raw_palette %>%
  data.frame() %>%
  mutate(id = row.names(.)) %>%
  left_join(hsv_raw_palette) %>%
  left_join(rgb_text_vector) %>%
  arrange(h,s,v) -> sorted_raw_palette

# if no override was requested for the default filter on low brightness we remove the
# lower tale of brightness distribution
brightness_stats <- boxplot.stats(sorted_raw_palette$v)
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

#conditional statement <- if categorical I sample distant colours, else I create a vector of n_of_colours
## around the mode of the distribution

if(type_of_variable == "categorical"){

  spaced_indexes <- seq(from=1, to =effective_n_of_color,by = effective_n_of_color/number_of_colors)
  final_palette <- sorted_raw_palette$hex_code[spaced_indexes]

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
      head(n=2) %>%  # we select the shortest distance available and the immediately subsequent record (colour)
      select(hex_code) %>%
      pull() %>%
      as.vector()-> hex_codes
    # we interpolate number_of_colours color between the nearest to the mode and the subsequent
     gradient_builder <- colorRampPalette(colors = hex_codes)
     final_palette <- gradient_builder(number_of_colors)




}else{
  stop("you must specify a valid token for type_of_variable argument")
}
return(final_palette)
}
