% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_palette.R
\name{create_palette}
\alias{create_palette}
\title{make a palette from your image}
\usage{
create_palette(
  image_path = NA,
  number_of_colors = 40,
  type_of_variable = NA,
  filter_on_low_brightness = TRUE,
  filter_on_high_brightness = TRUE,
  optimize_palette = TRUE,
  filter_on_saturation = TRUE
)
}
\arguments{
\item{image_path}{string,default to NA. relative path to the custom image, including .jpeg extension}

\item{type_of_variable}{string, default to 'categorical'. type of variable to be plotted with the building palette}

\item{filter_on_low_brightness}{boolean, default to true. specifies if a filter on colours with low brigthness should be applied to enhance the palette}

\item{filter_on_high_brightness}{boolean, default to true. specifies if a filter on colours with high brigthness should be applied to enhance the palette}

\item{optimize_palette}{boolean, default to true. specifies if palette optimization algorithm should be applied or not}

\item{filter_on_saturation}{boolean, default to ture. specifies if a filter on low saturation should be applied.}

\item{number_of_colours}{integer, default to 40. number of desired colours in the final palette, as specified by the user when calling create_palette}
}
\description{
processes a custom jpeg image producing a palett with a user-defined number of colours
}
\details{
palette creation and optimization ist started drawing a raw palette of rgb colours from the jpeg image provided.
The palette is then optimized applying the following four steps:
- conversion to hsv scale in order to easily elavorate on colour order and properties.
- filter on colours with a brightness lower than the first quartile of v distribution
- filter on colours with a brightness higher or equal to the Tukey's outlier threshold computed on the overall v distribution
- subset of the palette according to the type of variable to be plotted: a spaced sample in case of categorical variables, an interpolation between two colours close to the mode of h for continuous variables
}
\examples{
create_palette("data/nascita_venere.jpg",number_of_colors = 20, type_of_variable = "categorical")
}
\author{
Andrea Cirillo
}
