optimize_palette <- function(rgb_raw_palette = NA, number_of_colors =NA ,type_of_variable ="categorical",effective_n_of_color=NA){

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


#conditional statement <- if categorical I sample distant colours, else I create a vector of n_of_colours
## around the mode of the distribution

if(type_of_variable == "categorical"){

  spaced_indexes <- seq(from=1, to =effective_n_of_color,by = effective_n_of_color/number_of_colors)
  final_palette <- sorted_raw_palette$hex_code[spaced_indexes]

  }else if ( tipe_of_variable == "continous"){

}else{
  stop("you must specify a valid token for type_of_variable argument")
}

}
