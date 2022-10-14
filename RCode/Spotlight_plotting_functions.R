# Example of using glue to just paste the value into pre-created HTML block
# Example adapted from rtjohnson12 at: 
# https://github.com/renkun-ken/formattable/issues/79#issuecomment-573165954
library(gt)
library(formattable)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

##------------functions--------
# Example table with bar plot
#modified from :https://themockup.blog/static/slides/intro-tables.html?panelset14=table9#63
bar_chart <- function(value, color = "red", display_value = NULL){
  # Choose to display percent of total
  if (is.null(display_value)) {
    display_value <- "&nbsp;"
  } else {
    display_value <- display_value
  }
  # paste color and value into the html string
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> {display_value} </span>")
}

# create a color palette w/ paletteer
col_pal <- function(value, na.rm=TRUE){
  # set high and low
  #domain_range <- c(1,5)
  # create the color based of domain
  scales::col_numeric(
    rev(paletteer::paletteer_d("RColorBrewer::RdYlGn")) %>% as.character(), 
    domain = c(1,5), na.color = NA)(value)
}

