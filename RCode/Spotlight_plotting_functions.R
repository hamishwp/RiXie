library(gt)
library(formattable)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(kableExtra)


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


##more than two colors for color_tile in formattable...
#https://stackoverflow.com/a/49887341/454773
color_tile2 <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = ifelse(is.na(x),NA,csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                               byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3))))
  })}


##modify spec_color??
spec_color2 <- function(x, alpha = 1, begin = 0, end = 1,
                       direction = 1, option = "D",
                       na_color = "#BBBBBB", scale_from = NULL) {
  if (is.null(scale_from)) {
    x <- round(rescale(x, c(1, 256)))
  } else {
    x <- round(rescale(x, to = c(1, 256),
                       from = scale_from))
  }
  
  color_code <- viridisLite::viridis(256, alpha, begin, end, direction, option)[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}