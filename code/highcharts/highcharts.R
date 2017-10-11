
# Packages ------------------------------------------------------------------------------------

library("highcharter")


# Drilldown -----------------------------------------------------------------------------------

# drilldown on stacked grouped column chart

# data for the first level plot
elections_2010 <- data.frame(
  name = c("Republicans", "Democrats", "Others"),
  y = c(5, 2, 4),
  drilldown = tolower(paste0(c("Republicans", "Democrats", "Others"), "_elections_2010"))
) %>%
  list_parse
elections_2014 <- data.frame(
  name = c("Republicans", "Democrats", "Others"),
  y = c(4, 4, 4),
  drilldown = tolower(paste0(c("Republicans", "Democrats", "Others"), "_elections_2014"))
) %>%
  list_parse
intentions_2010 <- data.frame(
  name = c("Republicans", "Democrats", "Others"),
  y = c(5.5, 2.5, 4.5),
  drilldown = tolower(paste0(c("Republicans", "Democrats", "Others"), "_intentions_2010"))
) %>%
  list_parse
intentions_2014 <- data.frame(
  name = c("Republicans", "Democrats", "Others"),
  y = c(4.5, 4.5, 4.5),
  drilldown = tolower(paste0(c("Republicans", "Democrats", "Others"), "_intentions_2014"))
) %>%
  list_parse

# data for the second level
republicans_elections_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(4, 2, 1, 4)
) %>%
  list_parse2
democrats_elections_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(6, 2, 2, 4)
) %>%
  list_parse2
others_elections_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(2, 7, 3, 2)
) %>%
  list_parse2
republicans_elections_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(2, 4, 1, 7)
) %>%
  list_parse2
democrats_elections_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(4, 2, 5, 3)
) %>%
  list_parse2
others_elections_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(7, 8, 2, 2)
) %>%
  list_parse2

republicans_intentions_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(4.5, 2.5, 1.5, 4.5)
) %>%
  list_parse2
democrats_intentions_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(6.5, 2.5, 2.5, 4.5)
) %>%
  list_parse2
others_intentions_2010 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(2.5, 7.5, 3.5, 2.5)
) %>%
  list_parse2
republicans_intentions_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(2.5, 4.5, 1.5, 7.5)
) %>%
  list_parse2
democrats_intentions_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(4.5, 2.5, 5.5, 3.5)
) %>%
  list_parse2
others_intentions_2014 <- data.frame(
  name = c("East", "West", "North", "South"),
  y = c(7.5, 8.5, 2.5, 2.5)
) %>%
  list_parse2

# the plot
highchart() %>%
  hc_chart(
    type = 'column'
  ) %>%
  hc_title(
    text = 'Highcharts multi-series drilldown'
  ) %>%
  hc_xAxis(
    type = 'category'
  ) %>%
  hc_plotOptions(
    series = list(
      stacking = 'normal',
      borderWidth = 0,
      dataLabels = list(
        enabled = FALSE
      )
    )
  ) %>%
  hc_add_series(
    name = 'elections_2010',
    data = elections_2010,
    stack = 'elections'
  ) %>%
  hc_add_series(
    name = 'elections_2014',
    data = elections_2014,
    stack = 'elections'
  ) %>%
  hc_add_series(
    name = 'intentions_2010',
    data = intentions_2010,
    stack = 'intentions'
  ) %>%
  hc_add_series(
    name = 'intentions_2014',
    data = intentions_2014,
    stack = 'intentions'
  ) %>%
  hc_drilldown(
    allowPointDrilldown = FALSE,
    series = list(
      list(
        id = 'republicans_elections_2010',
        name = 'Republican 2010',
        data = republicans_elections_2010,
        stack = 'elections'
      ),
      list(
        id = 'democrats_elections_2010',
        name = 'Republican 2010',
        data = democrats_elections_2010,
        stack = 'elections'
      ),
      list(
        id = 'others_elections_2010',
        name = 'Other 2010',
        data = others_elections_2010,
        stack = 'elections'
      ),
      list(
        id = 'republicans_elections_2014',
        name = 'Republican 2014',
        data = republicans_elections_2014,
        stack = 'elections'
      ),
      list(
        id = 'democrats_elections_2014',
        name = 'Democrats 2014',
        data = democrats_elections_2014,
        stack = 'elections'
      ),
      list(
        id = 'others_elections_2014',
        name ='Other 2014',
        data = others_elections_2014,
        stack = 'elections'
      ),
      # stack intentions
      list(
        id = 'republicans_intentions_2010',
        name = 'Republican 2010',
        data = republicans_intentions_2010,
        stack = 'intentions'
      ),
      list(
        id = 'democrats_intentions_2010',
        name = 'Republican 2010',
        data = democrats_intentions_2010,
        stack = 'intentions'
      ),
      list(
        id = 'others_intentions_2010',
        name = 'Other 2010',
        data = others_intentions_2010,
        stack = 'intentions'
      ),
      list(
        id = 'republicans_intentions_2014',
        name = 'Republican 2014',
        data = republicans_intentions_2014,
        stack = 'intentions'
      ),
      list(
        id = 'democrats_intentions_2014',
        name = 'Democrats 2014',
        data = democrats_intentions_2014,
        stack = 'intentions'
      ),
      list(
        id = 'others_intentions_2014',
        name ='Other 2014',
        data = others_intentions_2014,
        stack = 'intentions'
      )
    )
  )

# drilldown chart : not working

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


# Population pyramid : not working
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
      categories = c('0-4', '5-9', '10-14', '15-19',
                     '20-24', '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90-94',
                     '95-99', '100 + '),
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


# column range : not working
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


# Pie chart with predefined labels ------------------------------------------------------------

data_plot <- data.table(name = letters[1:5],
                        y = round(runif(n = 5), digits = 1),
                        labels = round(runif(n = 5, min = -5, max = 5), digits = 1))

highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(data = data_plot,
                dataLabels = list(format = "{point.y} (vs {point.labels} N-1)"),
                enabled = TRUE) %>%
  hc_plotOptions(pie = list(showInLegend = TRUE))


# Custom funnel plot --------------------------------------------------------------------------

# area range
plot_data <- data.frame(
  name = paste0("Step_", 1:5),
  low = c(20, 30, 45, 55, 65),
  high = c(120, 110, 95, 85, 75),
  color = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50")
) %>%
  list_parse


highchart() %>%
  hc_chart(type = 'arearange') %>%
  hc_add_series(
    name = "funnel",
    data = list(
      list(
      x = 1,
      low = 7,
      high = 8,
      fillColor = "red"
    ),
    list(
      x = 2,
      low = 6,
      high = 7,
      fillColor = "blue"
    )
    )
  )






# with bubble
data_plot <- data.table(name = paste0("step", 1:3),
                        x = 0:2,
                        y = 1,
                        z = c(100, 20, 10),
                        percent = c(100, 20, 50),
                        nb_indiv = c(2000, 1000, 200))

data_plot1 <- data.table(name = paste0("step", 1:3),
                         x = 0:2,
                         y = 2,
                         z = c(100, 20, 10),
                         percent = c(100, 20, 50),
                         nb_indiv = c(2000, 1000, 200))

list_plot <- c(list_parse(data_plot), list(NA), list_parse(data_plot1))
list_plot <- c(list_parse(data_plot), list_parse(data_plot1))

highchart() %>%
  hc_chart(type = "bubble",
           inverted = TRUE) %>%
  hc_xAxis(categories = data_plot$name,
           gridLineDashStyle = "dash",
           gridLineWidth = 1,
           lineWidth = 0,
           tickWidth = 0,
           tickmarkPlacement = 'on') %>%
  hc_yAxis(visible = FALSE) %>%
  hc_add_series(data = list_plot) %>%
  hc_plotOptions(bubble = list(lineWidth = 2,
                               minSize = "15%",
                               maxSize = "30%",
                               showInLegend = FALSE,
                               marker = list(fillColor = NULL,
                                             fillOpacity = 1),
                               dataLabels = list(enabled = TRUE,
                                                 formatter = JS('function(){
                                                      if(this.point.x == 0) {
                                                        return this.point.nb_indiv;
                                                      } else{
                                                        return this.point.percent + "%";
                                                      }
                                                 }'),
                                                 style = list(textOutline = 'none'),
                                                 inside = TRUE,
                                                 align = "center"))) %>%
  hc_tooltip(useHTML = TRUE,
             headerFormat = "<table>",
             pointFormat = paste("<tr>
                                      <th>{point.name} :</th>
                                 </tr>",
                                 "<tr>
                                      <td>{point.percent}%</td>
                                 </tr>"),
             footerFormat = "</table>")


data_plot <- data.table(name = paste0("step", 1:3),
                        x = 0:2,
                        y = 0,
                        z = c(100, 20, 10),
                        percent = c(100, 20, 50),
                        nb_indiv = c(2000, 1000, 200))

data_plot1 <- data.table(name = paste0("step", 1:3),
                         x = 0:2,
                         y = 1,
                         z = c(100, 20, 10),
                         percent = c(100, 20, 50),
                         nb_indiv = c(2000, 1000, 200))

data_plot2 <- data.table(name = paste0("step", 1:3),
                         x = 0:2,
                         y = 2,
                         z = c(100, 20, 10),
                         percent = c(100, 20, 50),
                         nb_indiv = c(2000, 1000, 200))

data_plot3 <- data.table(name = paste0("step", 1:3),
                         x = 0:2,
                         y = 3,
                         z = c(100, 20, 10),
                         percent = c(100, 20, 50),
                         nb_indiv = c(2000, 1000, 200))

highchart() %>%
  hc_chart(type = "bubble",
           inverted = TRUE) %>%
  hc_xAxis(categories = data_plot$name,
           gridLineDashStyle = "dash",
           gridLineWidth = 1,
           lineWidth = 0,
           tickWidth = 0,
           tickmarkPlacement = 'on') %>%
  hc_yAxis(
    categories = c("global", "model1", "model2", "model3"),
    gridLineWidth = 0,
    lineWidth = 0,
    tickWidth = 0,
    showFirstLabel = FALSE,
    showLastLabel = FALSE,
    opposite = TRUE
  ) %>%
  hc_add_series(data = data_plot) %>%
  hc_add_series(data = data_plot1) %>%
  hc_add_series(data = data_plot2) %>%
  hc_add_series(data = data_plot3) %>%
  hc_plotOptions(bubble = list(lineWidth = 2,
                               minSize = "15%",
                               maxSize = "30%",
                               showInLegend = FALSE,
                               marker = list(fillColor = NULL,
                                             fillOpacity = 1),
                               dataLabels = list(enabled = TRUE,
                                                 formatter = JS('function(){
                                                      if(this.point.x == 0) {
                                                        return this.point.nb_indiv;
                                                      } else{
                                                        return this.point.percent + "%";
                                                      }
                                                 }'),
                                                 style = list(textOutline = 'none'),
                                                 inside = TRUE,
                                                 align = "center"))) %>%
  hc_tooltip(useHTML = TRUE,
             headerFormat = "<table>",
             pointFormat = paste("<tr>
                                      <th>{point.name} :</th>
                                 </tr>",
                                 "<tr>
                                      <td>{point.nb_indiv} leads</td>
                                 </tr>"),
             footerFormat = "</table>")


# Solid gauge ---------------------------------------------------------------------------------

highchart(width = '100%', height = '100%') %>%
  hc_chart(type = "solidgauge",
           spacing = 0) %>%
  hc_title(text = "Activity",
           style = list(fontSize = "24px")) %>%
  hc_tooltip(borderWidth = 0,
             backgroundColor = 'none',
             shadow = FALSE,
             style = list(fontSize = '16px'),
             pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
             positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 120%};}")
  ) %>%
  hc_pane(startAngle = 0,
          endAngle = 360,
          background = list(
            list(outerRadius = '112%',
                 innerRadius = '88%',
                 backgroundColor = JS("Highcharts.Color(Highcharts.getOptions().colors[0]).setOpacity(0.1).get()"),
                 borderWidth =  0),
            list(outerRadius = '87%',
                 innerRadius = '63%',
                 backgroundColor = JS("Highcharts.Color(Highcharts.getOptions().colors[1]).setOpacity(0.1).get()"),
                 borderWidth = 0),
            list(outerRadius = '62%',
                 innerRadius =  '38%',
                 backgroundColor = JS("Highcharts.Color(Highcharts.getOptions().colors[2]).setOpacity(0.1).get()"),
                 borderWidth = 0))) %>%
  hc_yAxis(min = 0,
           max = 100,
           lineWidth = 0,
           tickPositions = list()) %>%
  hc_plotOptions(solidgauge = list(borderWidth = '34px',
                                   dataLabels = list(enabled = FALSE,
                                                     verticalAlign = 'bottom',
                                                     align = 'center'),
                                   linecap = 'round',
                                   stickyTracking = FALSE)) %>%
  hc_add_series(name = "Move",
                borderColor = JS("Highcharts.getOptions().colors[0]"),
                data = list(list(color = JS("Highcharts.getOptions().colors[0]"),
                                 radius = "100%",
                                 innerRadius = "100%",
                                 y = 80))) %>%
  hc_add_series(name = "Exercise",
                borderColor = JS("Highcharts.getOptions().colors[1]"),
                data = list(list(color = JS("Highcharts.getOptions().colors[1]"),
                                 radius = "75%",
                                 innerRadius = "75%",
                                 y = 65))) %>%
  hc_add_series(name = "Stand",
                borderColor = JS("Highcharts.getOptions().colors[2]"),
                data = list(list(color = JS("Highcharts.getOptions().colors[2]"),
                                 radius = "50%",
                                 innerRadius = "50%",
                                 y = 50)))
















