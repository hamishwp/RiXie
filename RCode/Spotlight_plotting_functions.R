# Example of using glue to just paste the value into pre-created HTML block
# Example adapted from rtjohnson12 at: 
# https://github.com/renkun-ken/formattable/issues/79#issuecomment-573165954

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
# note you could just pass a single color directly to the bar_chart function
col_pal <- function(value){
  
  # set high and low
  domain_range <- range(c(rule10_data$`2013`, rule10_data$`2017`))
  
  # create the color based of domain
  scales::col_numeric(
    paletteer::paletteer_d("ggsci::blue_material") %>% as.character(), 
    domain = c(min(value), max(value))
  )(value)
}

# BARPLOT

bar_yields <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% c(
      country_sel, "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
      "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
    )
  ) %>% 
  pivot_wider(names_from = year, values_from = yield) %>%  
  select(-crop) %>% 
  rowwise() %>% 
  mutate(
    mean = mean(c(`2013`, `2014`, `2015`, `2016`, `2017`))
  ) %>% 
  ungroup() %>% 
  select(Country, `2013`, `2017`, `mean`) %>% 
  mutate(
    bar = round(mean/max(mean)*100, digits = 2),
    color = col_pal(bar),
    bar_chart = bar_chart(bar, color = color),
    bar_chart = map(bar_chart, ~gt::html(as.character(.x)))) %>% 
  select(-bar, -color)

# BARPLOT

rule10_bar <- bar_yields %>% 
  gt() %>% 
  cols_width(vars(bar_chart) ~ px(100),
             vars(`2013`) ~ px(75),
             vars(`2017`) ~ px(75)
  ) %>% 
  cols_label(
    mean = md("Average<br>2013-17"),
    bar_chart = ""
  ) %>% 
  cols_align(
    align = "right",
    columns = 2:4
  ) %>% 
  cols_align(
    align = "left",
    columns = vars(bar_chart)
  ) %>% 
  fmt_number(2:4) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_labels(everything())
    )
  ) %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    table_body.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3)
  ) %>% 
  tab_footnote(footnote = "Potato Yield in Tonnes per Hectare",
               locations = cells_column_labels(
                 columns =2:4
               )) %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))