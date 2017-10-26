
# Packages ------------------------------------------------------------------------------------

library("data.table")
library("highcharter")
source("code/highcharts/functions.R")


# Column and bar chart data -------------------------------------------------------------------

# initial data
star_wars_raw <- httr::content(httr::GET("http://swapi.co/api/films/?format=json"))

# coerce to data frame
star_wars <- rbindlist(
  lapply(
    X = star_wars_raw$results,
    FUN = function(x){
      data.table(title = x$title,
                 characters = length(x$characters),
                 planets = length(x$planets),
                 starships = length(x$starships),
                 species = length(x$species),
                 release_date = x$release_date)
    }
  )
)

# order by release_date so the movies are in proper order in the graphs
setorderv(star_wars, cols = "release_date")


# Column and bar chart: classic ----------------------------------------------------------------

# grouped columns, user defined colors, axis labels styled, title styled, credits
highchart() %>%
  hc_add_series(data = star_wars$species, name = "Species",
                type = "column", color = "#e5b13a") %>%
  hc_add_series(data = star_wars$planets, name = "Planets",
                type = "column", color = "rgba(69, 114, 167, 0.5)") %>%
  hc_xAxis(categories = star_wars$title,
           title = list(text = "Movie"),
           labels = list(rotation = -45,
                         format = "Movie: {value}",
                         style = list(fontSize = "12px",
                                      fontFamily = "Verdana, sans-serif"))) %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_title(text = "Diversity in <span style=\"color:#e5b13a\">
           STAR WARS</span> movies",
           useHTML = TRUE) %>%
  hc_credits(enabled = TRUE, text = "Source: SWAPI",
             href = "https://swapi.co/",
             style = list(fontSize = "12px"))


# grouped bar chart, user defined colors, inverted axis, title styled, credits
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_add_series(data = star_wars$species, name = "Species",
                color = "#e5b13a") %>%
  hc_add_series(data = star_wars$planets, name = "Planets",
                color = "rgba(69, 114, 167, 0.5)") %>%
  hc_xAxis(categories = star_wars$title,
           title = list(text = "Movie")) %>%
  hc_yAxis(title = list(text = "Number"),
           opposite = TRUE,
           tickInterval = 2) %>%
  hc_title(text = "Diversity in <span style=\"color:#e5b13a\">
           STAR WARS</span> movies",
           useHTML = TRUE) %>%
  hc_credits(enabled = TRUE, text = "Source: SWAPI",
             href = "https://swapi.co/",
             style = list(fontSize = "12px"))


# stacked column chart, user defined colors, inverted axis, title styled, credits
highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_add_series(data = star_wars$species, name = "Species",
                color = "#e5b13a") %>%
  hc_add_series(data = star_wars$planets, name = "Planets",
                color = "rgba(69, 114, 167, 0.5)") %>%
  hc_xAxis(categories = star_wars$title,
           title = list(text = "Movie")) %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_title(text = "Diversity in <span style=\"color:#e5b13a\">
           STAR WARS</span> movies",
           useHTML = TRUE) %>%
  hc_credits(enabled = TRUE, text = "Source: SWAPI",
             href = "https://swapi.co/",
             style = list(fontSize = "12px"))


# stacked percent column chart, user defined colors, inverted axis, title styled, credits
highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(series = list(stacking = "percent")) %>%
  hc_add_series(data = star_wars$species, name = "Species",
                color = "#e5b13a") %>%
  hc_add_series(data = star_wars$planets, name = "Planets",
                color = "rgba(69, 114, 167, 0.5)") %>%
  hc_xAxis(categories = star_wars$title,
           title = list(text = "Movie")) %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_title(text = "Diversity in <span style=\"color:#e5b13a\">
           STAR WARS</span> movies",
           useHTML = TRUE) %>%
  hc_credits(enabled = TRUE, text = "Source: SWAPI",
             href = "https://swapi.co/",
             style = list(fontSize = "12px"))


# add multiple series at the same time
star_wars_series <- toHighchartList(star_wars, by = c("characters", "planets", "starships", "species"))

color <- c("#4AA942", "rgba(69, 114, 167, 0.5)", "#4bd5ee", "#e5b13a")
star_wars_series <- addArgument(star_wars_series, color)

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Diversity in <span style=\"color:#e5b13a\">
           STAR WARS</span> movies",
           useHTML = TRUE) %>%
  hc_subtitle(text = "Movies sorted by release date") %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_xAxis(categories = star_wars$title,
           title = list(text = "Movie")) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = NULL,
    enableMouseTracking = TRUE)
  ) %>%
  hc_add_series_list(star_wars_series)
rm(star_wars_series, color)


# grouped and stacked column chart
highchart() %>%
  hc_chart(type = 'column') %>%
  hc_title(text = 'Total fruit consumtion, grouped by gender') %>%
  hc_xAxis(categories = c('Apples', 'Oranges', 'Pears', 'Grapes', 'Bananas')) %>%
  hc_yAxis(
    allowDecimals = FALSE,
    min = 0,
    title = list(text = 'Number of fruits')
  ) %>%
  hc_tooltip(
    formatter = JS(
      "function () {
      return '<b>' + this.x + '</b><br/>' +
      this.series.name + ': ' + this.y + '<br/>' +
      'Total: ' + this.point.stackTotal;
      }"
    )
    ) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_add_series(name = 'John',
                data = c(5, 3, 4, 7, 2),
                stack = 'male') %>%
  hc_add_series(name = 'Joe',
                data = c(3, 4, 4, 2, 5),
                stack = 'male') %>%
  hc_add_series(name = 'Jane',
                data = c(2, 5, 6, 2, 1),
                stack = 'female') %>%
  hc_add_series(name = 'Janet',
                data = c(3, 0, 4, 4, 3),
                stack = 'female')

# Fixed placement
highchart() %>%
  hc_chart(type = 'column') %>%
  hc_title(text = 'Efficiency Optimization by Branch') %>%
  hc_xAxis(categories = c('Seattle HQ', 'San Francisco', 'Tokyo')) %>%
  hc_yAxis_multiples(
    list(min = 0,
         title = list(text = 'Employees')
    ),
    list(
      title = list(text = 'Profit (millions)'),
      opposite = TRUE
    )
  ) %>%
  hc_legend(shadow = FALSE) %>%
  hc_tooltip(shared = TRUE) %>%
  hc_plotOptions(
    column = list(
      grouping = FALSE,
      shadow = FALSE,
      borderWidth = 0
    )
  ) %>%
  hc_add_series(
    name = 'Employees',
    color = 'rgba(165,170,217,1)',
    data = c(150, 73, 20),
    pointPadding = 0.3,
    pointPlacement = -0.2
  ) %>%
  hc_add_series(
    name = 'Employees Optimized',
    color = 'rgba(126,86,134,.9)',
    data = c(140, 90, 40),
    pointPadding = 0.4,
    pointPlacement = -0.2
  ) %>%
  hc_add_series(
    name = 'Profit',
    color = 'rgba(248,161,63,1)',
    data = c(183.6, 178.8, 198.5),
    tooltip = list(
      valuePrefix = '$',
      valueSuffix = ' M'
    ),
    pointPadding = 0.3,
    pointPlacement = 0.2,
    yAxis = 1
  ) %>%
  hc_add_series(
    name = 'Profit Optimized',
    color = 'rgba(186,60,61,.9)',
    data = c(203.6, 198.8, 208.5),
    tooltip = list(
      valuePrefix = '$',
      valueSuffix = ' M'
    ),
    pointPadding = 0.4,
    pointPlacement = 0.2,
    yAxis = 1
  )

