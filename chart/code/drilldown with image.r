
# images are downloaded and resized with http://resizeimage.net/. Then they are hosted on github and the path is used with raw.

# Packages ----------------------------------------------------------------

library('highcharter')
library('data.table')


# Data --------------------------------------------------------------------

browser <- readRDS('chart/input/browser.rds')

# level1
level1 <- browser[, .(y = sum(data)), by = name]
level1$drilldown <- level1$name

# level2
level2 <- lapply(X = level1$name,
                 FUN = function(x){
                   id <- tolower(x)
                   data <- browser[name == x]
                   data <- data[, .(name = categories, y = data)]
                   data <- list_parse2(data)
                   return(list(id = id, name = 'Total share', data = data))
                 })


# Plot --------------------------------------------------------------------

highchart() %>%
  hc_chart(type = 'pie') %>%
  hc_add_series(name = 'Total share',data = list_parse(level1)) %>%
  hc_drilldown(
    activeDataLabelStyle = list(
      textDecoration = 'none',
      fontStyle = 'none'
    ),
    series = level2
  ) %>%
  hc_plotOptions(
    series = list(
      borderWidth = 0
    ),
    pie = list(
      innerSize = '50%',
      dataLabels = list(
        enabled = TRUE,
        format = '<b>{point.name}</b>: {point.y}'
      )
    )
  ) %>%
  hc_chart(
    backgroundColor = NULL,
    events = list(
      drilldown = JS(
        "function(e){
        var image = 'none';
        if (e.point.name == 'chrome') {
        image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/chrome.png';
        } else if (e.point.name == 'msie') {
        image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/ie.png';
        } else if (e.point.name == 'firefox') {
        image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/firefox.png';
        } else if (e.point.name == 'safari') {
        image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/safari.png';
        } else if (e.point.name == 'opera') {
        image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/opera.png';
        }
        $('#browser_plot').css('background', 'url(' + image + ') no-repeat 50% 50%').css('background-size', '15%');
        }"
      ),
      drillup = JS(
        "function(e) {
        $('#browser_plot').css('background-image', 'none');
        }"
      )
    )
  ) %>%
  hc_elementId(id = 'browser_plot')