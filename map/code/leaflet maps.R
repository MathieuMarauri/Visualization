
# Leaflet maps from : https://rstudio.github.io/leaflet/
# everything is explained with more details on the github page

# Packages ----------------------------------------------------------------

library('leaflet')
library('sp')
library('maps')
library('rgdal')
library('spdplyr')
library('data.table')


# Basic map ---------------------------------------------------------------

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R")
m  # Print the map


# The map widget ----------------------------------------------------------

# https://rstudio.github.io/leaflet/map_widget.html

# Set value for the minZoom and maxZoom settings,
# all options are available here : http://leafletjs.com/reference-1.0.0.html#map-option
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

# add some circles to a map
df <- data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(data = df) %>% addCircles()
# equivalent to
leaflet(data = df) %>% addCircles(lng = ~Long, lat = ~Lat)
# and to
leaflet() %>% addCircles(data = df)
leaflet() %>% addCircles(data = df, lat = ~ Lat, lng = ~ Long)

# using sp to create polygons
Sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 <- Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 <- Polygons(list(Sr1), "s1")
Srs2 <- Polygons(list(Sr2), "s2")
Srs3 <- Polygons(list(Sr4, Sr3), "s3/4")
SpP <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

# using maps
mapStates <- map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>%
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

# formula
m <- leaflet() %>% addTiles()
df <- data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m <- leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))


# Using basemaps ----------------------------------------------------------

# https://rstudio.github.io/leaflet/basemaps.html

# default is openstreetmap
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

# other basemaps are also available
names(providers)
m %>% addProviderTiles(providers$Stamen.Toner)
m %>% addProviderTiles(providers$CartoDB.Positron)
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)

# wms tiles
leaflet() %>%
  addTiles() %>%
  setView(-93.65, 42.0285, zoom = 4) %>%
  addWMSTiles(
    baseUrl = "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

# combining layers
m %>% addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)


# Markers -----------------------------------------------------------------

# https://rstudio.github.io/leaflet/markers.html

# icon markers
data(quakes)
# Show first 20 rows from the `quakes` dataset
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

# custom icon marker
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)
leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = greenLeafIcon)
# check on eiffel tower where is the center of the icon, it is at the bottom of the leaf
leaflet() %>% addTiles() %>%
  addMarkers(lng =2.2944813, lat = 48.8583701, icon = greenLeafIcon)

# several icons ...
quakes1 <- quakes[1:10,]
leafIcons <- icons(
  iconUrl = ifelse(quakes1$mag < 4.6,
                   "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)
leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons)

# ... with really different parameters
# Make a list of icons. We'll index into it based on name.
oceanIcons <- iconList(
  ship = makeIcon(iconUrl = "map/input/ferry.png", iconWidth = 18, iconHeight = 18),
  pirate = makeIcon(iconUrl = "map/input/pirate.png", iconWidth = 24, iconHeight = 24)
)
# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)
leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])

# using awesome icon
# first 20 quakes
df.20 <- quakes[1:20,]
getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)
leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))


# marker clusters
leaflet(quakes) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)


# circle markers
leaflet(df) %>% addTiles() %>% addCircleMarkers()
# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )


# Popups and labels -------------------------------------------------------

# https://rstudio.github.io/leaflet/popups.html

# popups
content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

leaflet() %>%
  addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE)
  )

# on markers
df <- read.csv(textConnection(
  "Name,Lat,Long
Samurai Noodle,47.597131,-122.327298
Kukai Ramen,47.6154,-122.327157
Tsukushinbo,47.59987,-122.326726"
))

leaflet(df) %>%
  addTiles() %>%
  addMarkers(~Long, ~Lat, popup = ~htmltools::htmlEscape(Name))

# labels
leaflet(df) %>%
  addTiles() %>%
  addMarkers(~Long, ~Lat, label = ~htmltools::htmlEscape(Name))

# customizing marker label
# Change Text Size and text Only and also a custom CSS
leaflet() %>%
  addTiles() %>%
  setView(-118.456554, 34.09, 13) %>%
  addMarkers(
    lng = -118.456554, lat = 34.105,
    label = "Default Label",
    labelOptions = labelOptions(noHide = T)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.095,
    label = "Label w/o surrounding box",
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.085,
    label = "label w/ textsize 15px",
    labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
  addMarkers(
    lng = -118.456554, lat = 34.075,
    label = "Label w/ custom CSS style",
    labelOptions = labelOptions(
      noHide = T, direction = "bottom",
      style = list(
        "color" = "red",
        "font-family" = "serif",
        "font-style" = "italic",
        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        "font-size" = "12px",
        "border-color" = "rgba(0,0,0,0.5)"
      )
    )
  )


# Lines and shapes --------------------------------------------------------

# https://rstudio.github.io/leaflet/shapes.html

# data from https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR(dsn = "map/input/cb_2016_us_state_20m/cb_2016_us_state_20m.shp",
                  layer = "cb_2016_us_state_20m",
                  GDAL1_integer64_policy = TRUE)
neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

# circles
cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )

# rectangles
leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )


# GeoJSOn and TopoJSON ----------------------------------------------------

# https://rstudio.github.io/leaflet/json.html

# data from http://eric.clst.org/Stuff/USGeoJSON
uscounties <- geojsonio::geojson_read("map/input/uscounties.geojson",
                                      what = "sp")

# filter on only ny county
nycounties <- uscounties %>% filter(STATE == 36)
rm(uscounties)

# counties population from
# https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents
counties_pop <- fread(file = 'map/input/counties_pop.csv')
counties_pop <- counties_pop[`State or district` == 'New York',
                             .(county_name = stringi::stri_replace_all_fixed(str = `County or equivalent`,
                                                                             pattern = ' County',
                                                                             replacement = ''),
                               pop = as.numeric(stringi::stri_replace_all_fixed(str = `2013 Pop`,
                                                                                pattern = ',',
                                                                                replacement = '')))]

nycounties@data <- merge(x = nycounties@data,
                         y = counties_pop,
                         by.x = 'NAME',
                         by.y = 'county_name')

pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop))
              # label = ~paste0(NAME, ": ", formatC(pop, big.mark = ","))
              ) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))


# Choropleths -------------------------------------------------------------

# From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read("map/input/us-states.geojson", what = "sp")

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(
  data = states,
  options = leafletOptions(zoomControl = FALSE,
                           minZoom = 4, maxZoom = 4,
                           dragging = FALSE)
  ) %>%
  setView(-96, 37.8, 4) %>%
  setMaxBounds(lng1 = -120, lng2 = -80, lat1 = 30, lat2 = 45) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")


