

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
  mutate(id = as.character(seq(1:number_of_colors)))-> rgb_text_vector

colnames(rgb_text_vector) <-  c("hex_code", "id")

rgb_raw_palette %>%
  data.frame() %>%
  mutate(id = row.names(.)) %>%
  left_join(hsv_raw_palette) %>%
  left_join(rgb_text_vector) %>%
  arrange(h,s,v) -> sorted_raw_palette
  show_col(sorted_raw_palette$hex_code)



 #we transpose and transform from RGB to HSV



# sort palette on hue saturation and luminosity

#conditional statement <- if categorical I sample distant colours, else near colours

if(type_of_variable = "categorical"){

}else if (tipe_of_variable = "continous")else{
  stop("you must specify a valid token for type_of_variable argument")
}