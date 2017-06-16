
# Exampels of grpahs with highcharts. From simple to more advanced with tweaks.

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


# drilldown chart

# add diversity level to then drill down to specific categories (planets, ...)
star_wars[, diversity := sum(.SD), by = title, .SDcols = c("characters", "planets", "starships", "species")]

# add drilldown variable so the graph understands the link between level one and the subsequent levels; colnames are important
star_wars_drilldown_level1 <- star_wars[, .(name = title,
                                            y = diversity,
                                            drilldown = tolower(title))]
# parse the table so it is understandable by highchart
star_wars_drilldown_level1 <- list_parse(star_wars_drilldown_level1)

# second level of the drilldown: the list variable contains parsed tables with the data that will be dispalyed in each drilldown
star_wars_drilldown_level2 <- star_wars[
  ,
  .(list = list(
    list_parse2(
      data.frame(
        name = c("characters", "planets", "starships", "species"),
        value = as.numeric(.SD),
        stringsAsFactors = FALSE
      )
    )
  ),
  id = tolower(title)),
  by = title,
  .SDcols = c("characters", "planets", "starships", "species")
  ]

# create the list with all the drilldown values formated to be understood by hc_drilldown series
star_wars_drilldown_level2 <- lapply(star_wars_drilldown_level2$id,
                                     FUN = function(x){
                                       list(id = x,
                                            data = star_wars_drilldown_level2[id == x, ]$list1[[1]])
                                     })

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "drilldown") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_add_series(
    name = "Diversity",
    data = star_wars_drilldown_level1,
    type = "column"
  ) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = star_wars_drilldown_level2
  )
rm(star_wars_drilldown_level1, star_wars_drilldown_level2)


# drilldown example with 3 levels
# 3 levels drilldown
highchart() %>%
  hc_chart(type="column") %>%
  hc_xAxis(type="category") %>%
  hc_add_series(
    name = "Things",
    data = list(
      list(
        name = "Animals",
        y = 10,
        drilldown = "animals"
      )
    )
  ) %>%
  hc_drilldown(
    series = list(
      list(
        name = "Animals",
        id = "animals",
        data = list(
          list(
            name = "Cats",
            y = 2,
            drilldown = "cats"
          )
        )
      ),
      list(
        name = "Cats",
        id = "cats",
        data = list(
          list(name = "white cats",
               y = 2),
          list(name = "black cats",
               y = 3),
          list(name = "red cats",
               y = 4))
      )
    )
  )

# Population pyramid
highchart() %>%
  hc_title(text = "Population pyramid") %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(
    list(
      categories = c('0-4', '5-9', '10-14', '15-19',
                     '20-24', '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90-94',
                     '95-99', '100 + '),
      reversed = FALSE,
      labels = list(step = 1)
    ),
    list(
      opposite = TRUE,
      reversed = FALSE,
      categories = categories,
      linkedTo = 0,
      labels = list(step = 1)
    )
  ) %>%
  hc_yAxis(title = list(text = NULL),
           labels = list(
             labelsformatter = JS(
               "function () {
                    return Math.abs(this.value) + '%';
                }"
             )
           )
  ) %>%
  hc_tooltip(formatter = JS("
                          function () {
                              return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
                                    'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
                          }"
  )) %>%
  hc_add_series(name = 'Male',
                data = c(-2.2, -2.2, -2.3, -2.5, -2.7, -3.1, -3.2,
                         -3.0, -3.2, -4.3, -4.4, -3.6, -3.1, -2.4,
                         -2.5, -2.3, -1.2, -0.6, -0.2, -0.0, -0.0)) %>%
  hc_add_series(name = 'Female',
                data = c(2.1, 2.0, 2.2, 2.4, 2.6, 3.0, 3.1, 2.9,
                         3.1, 4.1, 4.3, 3.6, 3.4, 2.6, 2.9, 2.9,
                         1.8, 1.2, 0.6, 0.1, 0.0))

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


# column range



highchart() %>%
  hc_chart(
    type = 'columnrange',
    inverted = TRUE
  ) %>%
  hc_title(
    text = 'Temperature variation by month'
  ) %>%
  hc_subtitle(
    text = 'Observed in Vik i Sogn, Norway'
  ) %>%
  hc_xAxis(
    categories = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  ) %>%
  hc_yAxis(
    title = list(
      text = 'Temperature ( °C )'
    )
  ) %>%
  hc_tooltip(
    valueSuffix = '°C'
  ) %>%
  hc_plotOptions(
    columnrange = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS("function () (
                          return this.y + '°C';
                       )"
        )
      )
    )
  ) %>%
  hc_legend(
    enabled = FALSE
  ) %>%
  hc_add_series(
    name = 'Temperatures',
    data = setNames(
      as.data.frame(
        rbind(
          c(-9.7, 9.4),
          c(-8.7, 6.5),
          c(-3.5, 9.4),
          c(-1.4, 19.9),
          c(0.0, 22.6),
          c(2.9, 29.5),
          c(9.2, 30.7),
          c(7.3, 26.5),
          c(4.4, 18.0),
          c(-3.1, 11.4),
          c(-5.2, 10.4),
          c(-13.5, 9.8)
        )
      ),
      c("low", "high")
    )
  )


setNames(as.data.frame(rbind(
  c(-9.7, 9.4),
  c(-8.7, 6.5),
  c(-3.5, 9.4),
  c(-1.4, 19.9),
  c(0.0, 22.6),
  c(2.9, 29.5),
  c(9.2, 30.7),
  c(7.3, 26.5),
  c(4.4, 18.0),
  c(-3.1, 11.4),
  c(-5.2, 10.4),
  c(-13.5, 9.8)
)), c("low", "high"))
