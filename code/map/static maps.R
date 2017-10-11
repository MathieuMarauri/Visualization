
# Examples of maps using R. Each section is self contained.

# to retrieve shape file by country: http://www.gadm.org/country
# geojson for France : https://github.com/gregoiredavid/france-geojson

# Packages ------------------------------------------------------------------------------------

library("data.table")
library("ggplot2")
library("osmplotr") # for osm plots section
import::from("magrittr", "%>%")


# Simple world map ----------------------------------------------------------------------------

# data for the world, and point for big cities

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# data for cities: http://simplemaps.com/data/world-cities
cities <- fread('input/simplemaps-worldcities-basic.csv')
cities <- cities[order(-pop), ][1:100, ]

# fortify the spatial polygon to a data frame by ISO2
world <- fortify(wrld_simpl, region = "ISO2")
world <- setDT(world)[!id %in% c("AQ", "GL"), ] # we don't need Antarctica and Grennland

# map
ggplot() +
  # setup base map
  geom_map(data = world,
           map = world,
           mapping = aes(map_id = id, x = long, y = lat),
           fill = "lightgrey",
           color = "black",
           size = 0.25) +
  # cities point by population size
  geom_point(data = cities,
             mapping = aes(x = lng, y = lat, size = pop),
             alpha = 0.5,
             color = "dodgerblue") +
  # fix axis to keep the the scale
  coord_map() +
  # remove axis name
  labs(x = "", y = "") +
  # only keep the map
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


rm(cities, world, wrld_simpl)


# World map with popuation by country ---------------------------------------------------------

# data for the world and country colored by population

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# fortify the spatial polygon to a data frame by ISO3 to link with the population data
world <- fortify(wrld_simpl, region = "ISO3")
world <- setDT(world)[!id %in% c("ATA", "GRL"), ] # we don't need Antarctica and Grennland

# population by country
population <- fread('https://raw.githubusercontent.com/datasets/population/master/data/population.csv')
population <- population[Year == 2015, ]
population <- population[`Country Code` %in% unique(world$id), ]

# map
ggplot() +
  # setup base map
  geom_map(data = world,
           map = world,
           aes(map_id = id, x = long, y = lat),
           fill = "lightgrey",
           color = "black",
           size = 0.25) +
  # fill country by population
  geom_map(data = population,
           map = world,
           mapping = aes(map_id = `Country Code`, fill = Value),
           color = "#7f7f7f",
           size = 0.25) +
  # specific palette
  scale_fill_continuous(low = "dodgerblue", high = "indianred") +
  # fix axis to keep the the scale
  coord_map() +
  # remove axis name
  labs(x = "", y = "") +
  # only keep the map
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

rm(population, world, wrld_simpl)


# French map  ---------------------------------------------------------------------------------

# Population by department
population_dt <- fread("input/repartition_departement.csv",
                       sep = ";")

# shape file of french department
shape_sdf <- readRDS("input/FRA_adm2.rds")
shape_df <- fortify(shape_sdf,
                    region = "NAME_2")

# department centroid and names to be plotted on the map
departement_centroid_df <- data.table(long = coordinates(shape_sdf)[, 1],
                                      lat = coordinates(shape_sdf)[, 2])
departement_centroid_df[, 'NAME_2'] <- shape_sdf@data[,'NAME_2']
departement_centroid_df[, 'CCA_2'] <- shape_sdf@data[,'CCA_2']

# add population to the shape file
shape_df <- merge(x = shape_df,
                  y = population_dt,
                  by.x = "id",
                  by.y = "departement",
                  all.x = TRUE)

# initialization of the map, the group argument removes the lines connecting points  that are not to be connected
plot <- ggplot(data = shape_df, mapping = aes(x = long, y = lat, group = group)) +
  # color by department depending on the population
  geom_polygon(mapping = aes(fill = cut(pop_totale, 9))) +
  # department border
  geom_path(colour = 'black') +
  # names of the department place in the center
  geom_text(data = departement_centroid_df,
            mapping = aes(label = NAME_2, x = long, y = lat, group = NAME_2),
            size = 2) +
  # special interest point
  geom_point(data = NULL, aes(x = 2.2944813, y = 48.8583701), shape = 23, size = 3, fill = "dodgerblue") +
  # keep axis scale
  coord_map() +
  # suppress axis labels
  labs(x = " ", y = " ") +
  # change colors
  scale_fill_brewer('', palette  = 'PuRd') +
  # suppress all elements but the map itself
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none',
        # transparent backgrounf to save map as png
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
plot

# save the plot
png("output/french_map.png", width = 8, height = 14, units = 'in', res = 600, bg = "transparent")
plot
dev.off()

rm(plot, shape_df, departement_centroid_df, population_dt, shape_sdf)


# Paris region map with image -----------------------------------------------------------------

# Population by department
population_dt <- fread("input/repartition_departement.csv",
                       sep = ";")

# shape file of french department
shape_sdf <- readRDS("input/FRA_adm2.rds")
shape_df <- fortify(shape_sdf,
                    region = "NAME_2")

# department centroid and names to be plotted on the map
departement_centroid_df <- data.table(long = coordinates(shape_sdf)[, 1],
                                      lat = coordinates(shape_sdf)[, 2])
departement_centroid_df[, 'NAME_2'] <- shape_sdf@data[,'NAME_2']
departement_centroid_df[, 'CCA_2'] <- shape_sdf@data[,'CCA_2']

# add population to the shape file
shape_df <- merge(x = shape_df,
                  y = population_dt,
                  by.x = "id",
                  by.y = "departement",
                  all.x = TRUE)

# filter on the proper region
shape_idf_df <- setDT(shape_df)[code %in% c(75, 77, 78, 91, 92, 93, 94, 95), ]
idf_centroid_df <- setDT(departement_centroid_df)[CCA_2 %in% c(75, 77, 78, 91, 92, 93, 94, 95), ]

# image instead of point
img <-  png::readPNG('input/eiffel_tower.png')
img <- grid::rasterGrob(img, interpolate = TRUE)

# initialization of the map, the group argument removes the lines connecting points  that are not to be connected
ggplot(data = shape_idf_df, mapping = aes(x = long, y = lat, group = group)) +
  # color by department depending on the population
  geom_polygon(mapping = aes(fill = cut(pop_totale, 9))) +
  # department border
  geom_path(colour = 'black') +
  # names of the department place in the center
  geom_text(data = idf_centroid_df,
            mapping = aes(label = NAME_2, x = long, y = lat, group = NAME_2),
            size = 2) +
  # fix the ration between axes
  coord_equal() +
  # special interest point as an image
  annotation_custom(grob = img,
                    xmin = 2.2944813 - 0.05, xmax = 2.2944813 + 0.05,
                    ymin = 48.8583701, ymax = 48.8583701 + 0.1) +
  # suppress axis labels
  labs(x = " ", y = " ") +
  # white background
  theme_bw() +
  # change colors
  scale_fill_brewer('', palette  = 'PuRd') +
  # suppress all elements but the map itself
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none',
        # transparent backgrounf to save map as png
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

rm(shape_idf_df, idf_centroid_df, img, shape_df, departement_centroid_df, population_dt, shape_sdf)


# Density map ---------------------------------------------------------------------------------

data_map <- fread('input/vilnius_point.csv')

map <- ggmap::get_map(location = "Vilnius",
                      zoom = 14,
                      maptype = "terrain",
                      source = "google",
                      color = "color")
ggmap::ggmap(map) +
  stat_density2d(data = data_map,
                 mapping = aes(x = Y,
                               y = X,
                               fill = ..level..),
                 alpha = 0.5,
                 geom = "polygon")


# osm plots -----------------------------------------------------------------------------------

# extract data, list of all key-value : http://wiki.openstreetmap.org/wiki/Map_Features

# Paris map
# paris_box <- osmdata::getbb("Paris")
paris_box <- get_bbox(c(2.22, 48.81, 2.41, 48.9))

# park
paris_park <- extract_osm_objects(
  key = 'leisure',
  value = 'park',
  bbox = paris_box,
  geom_only = TRUE
)

# river
paris_waterway <- extract_osm_objects(
  key = 'name',
  value = "River Seine",
  bbox = paris_box,
  geom_only = FALSE,
  return_type = "multipolygon"
)

# building
paris_waterway <- extract_osm_objects(
  key = 'building',
  bbox = paris_box,
  geom_only = TRUE
)


paris_map <- osm_basemap(bbox = paris_box, bg = 'gray80') %>%
  add_osm_objects(obj = paris_park, col = 'forestgreen') %>%
  add_osm_objects(obj = paris_waterway, col = 'royalblue')

print_osm_map(paris_map)


# extract specific building
extra_pairs <- list(c('addr:street', 'Stamford.St'),
                    c('addr:housenumber', '150'))
stamford <- extract_osm_objects(key = 'building',
                                 extra_pairs = extra_pairs,
                                 bbox = london_box)


# Subplots in map -----------------------------------------------------------------------------

#'
#' This function converts a ratser image to a data frame that can then be plotted.
#' It is extracted from 'inscaven' answer in https://stackoverflow.com/questions/28206611/adding-custom-image-to-geom-polygon-fill-in-ggplot
#'
#' @param image an image read with readPNG or equivalent
#' @param ymin the minimum y value of the polygon in wich the image will be plotted
#' @param ymax the maximum y value of the polygon in wich the image will be plotted
#' @param xmin the minimum x value of the polygon in wich the image will be plotted
#' @param xmax the maximum x value of the polygon in wich the image will be plotted
#'
#' @return the data frame equivalent of the image.
#'
imgToDF <- function(image, ymin = 0, ymax = 1, xmin = 0, xmax = 1) {
  import::from(magrittr, "%>%")
  if (dim(image)[3] > 3) hasalpha <- T else hasalpha <- F

  outMatrix <- matrix("#00000000", nrow = dim(image)[1], ncol = dim(image)[2])

  for (i in 1:dim(image)[1])
    for (j in 1:dim(image)[2])
      outMatrix[i, j] <- rgb(image[i,j,1], image[i,j,2], image[i,j,3], ifelse(hasalpha, image[i,j,4], 1))

  colnames(outMatrix) <- seq(1, ncol(outMatrix))
  rownames(outMatrix) <- seq(1, nrow(outMatrix))
  as.data.frame(outMatrix) %>% dplyr::mutate(Y = nrow(outMatrix):1) %>% tidyr::gather(X, color, -Y) %>%
    dplyr::mutate(X = xmin + as.integer(as.character(X))*(xmax-xmin)/ncol(outMatrix), Y = ymin + Y*(ymax-ymin)/nrow(outMatrix))
}

# plot 1: european map with flag

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# fortify the spatial polygon to a data frame by ISO3 to link with the population data
world <- fortify(wrld_simpl, region = "ISO3")
setDT(world)
rm(wrld_simpl)

# europe limits
europe_limits <- c(xmin = -15, xmax = 20, ymin = 35, ymax = 60)

# flag to fill countries
flag <- png::readPNG(RCurl::getURLContent("http://www.drapeauxdespays.fr/data/flags/ultra/pt.png"))
# the image of the flag is transformed as a data frame so it can be added to the plot
flag <- imgToDF(image = flag,
                ymin = min(world[group == 'PRT.1',]$lat),
                ymax = max(world[group == 'PRT.1',]$lat),
                xmin = min(world[group == 'PRT.1',]$long),
                xmax = max(world[group == 'PRT.1',]$long))
# only points in the portuguese polygon are kept
flag <- flag[as.logical(point.in.polygon(point.x = flag$X,
                                         point.y = flag$Y,
                                         pol.x = world[group == 'PRT.1',]$long,
                                         pol.y = world[group == 'PRT.1',]$lat)), ]

ggplot() +
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group)) +
  geom_tile(data = flag, aes(x = X, y = Y), fill = flag$color) +
  coord_equal(xlim = europe_limits[c("xmin", "xmax")],
              ylim = europe_limits[c("ymin", "ymax")])



# plot 2: portugal map from google map



# add special font
myfont <- "Roboto Condensed"

# load the already prepared data
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/df-27-261-urb-rur.RData"))
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/spatial-27-261.RData"))


# fortify spatial objects
bord <- fortify(Sborders)
fort <- fortify(Sn2, region = "id")
neighbors <- fortify(Sneighbors)

fort_map <- left_join(df,fort,"id")

# create a blank map
basemap <- ggplot()+
        geom_polygon(data = neighbors, aes(x = long, y = lat, group = group),
                     fill = "grey90",color = "grey90")+
        coord_equal(ylim = c(1350000,5450000), xlim = c(2500000, 6600000))+
        theme_map(base_family = myfont)+
        theme(panel.border = element_rect(color = "black",size = .5,fill = NA),
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15))+
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(x = NULL, y = NULL)

ggsave(filename = "basemap.png", basemap, width = 5, height = 5)



# create a nice mosaic plot; solution from SO:
# http://stackoverflow.com/a/19252389/4638884
makeplot_mosaic <- function(data, x, y, ...){
        xvar <- deparse(substitute(x))
        yvar <- deparse(substitute(y))
        mydata <- data[c(xvar, yvar)];
        mytable <- table(mydata);
        widths <- c(0, cumsum(apply(mytable, 1, sum)));
        heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});

        alldata <- data.frame();
        allnames <- data.frame();
        for(i in 1:nrow(mytable)){
                for(j in 1:ncol(mytable)){
                        alldata <- rbind(alldata, c(widths[i],
                                                    widths[i+1],
                                                    heights[j, i],
                                                    heights[j+1, i]));
                }
        }
        colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

        alldata[[xvar]] <- rep(dimnames(mytable)[[1]],
                               rep(ncol(mytable), nrow(mytable)));
        alldata[[yvar]] <- rep(dimnames(mytable)[[2]], nrow(mytable));

        ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
                geom_rect(color="white", aes_string(fill=yvar)) +
                xlab(paste(xvar, "(count)")) +
                ylab(paste(yvar, "(proportion)"));
}

typ_mosaic <- makeplot_mosaic(data = df %>% mutate(type = as.numeric(type)),
                              x = subregion, y = type)+
        theme_void()+
        scale_fill_viridis(option = "B", discrete = T, end = .8)+
        scale_y_continuous(limits = c(0, 1.4))+
        annotate("text",x = c(27, 82.5, 186), y = 1.05,
                 label=c("EAST", "SOUTH", "WEST"),
                 size = 4, fontface = 2,
                 vjust = 0.5, hjust = 0,
                 family = myfont) +
        coord_flip()+
        theme(legend.position = "none")

ggsave(filename = "mosaic.png", typ_mosaic, width = 4, height = 3)

# a nice small function to overcome some mapping problems with nested polygons
# see more at SO
# https://stackoverflow.com/questions/21748852
gghole <- function (fort) {
        poly <- fort[fort$id %in% fort[fort$hole, ]$id, ]
        hole <- fort[!fort$id %in% fort[fort$hole, ]$id, ]
        out <- list(poly, hole)
        names(out) <- c("poly", "hole")
        return(out)
}

# annotate a small map of the subregions of Europe
an_sub <- basemap +
        geom_polygon(data = gghole(fort_map)[[1]],
                     aes(x = long, y = lat, group = group, fill = subregion),
                     color = NA)+
        geom_polygon(data  =  gghole(fort_map)[[2]],
                     aes(x = long, y = lat, group = group, fill = subregion),
                     color = NA)+
        scale_fill_manual(values = rev(brbg3)) +
        theme(legend.position = "none")

ggsave(filename = "sub.png", an_sub, width = 4, height = 4)




# carte ile de france

# data filter on ile de france
departement_shape_df_idf <- setDT(departement_shape_df)[code %in% c(75, 77, 78, 91, 92, 93, 94, 95), ]
departement_centroid_idf <- setDT(departement_centroid_df)[CCA_2 %in% c(75, 77, 78, 91, 92, 93, 94, 95), ]

p <- ggplot(data = departement_shape_df_idf,
            mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(mapping = aes(fill = cut(potentiel, 8))) +
  geom_text_repel(data = departement_centroid_idf,
                  mapping = aes(label = NAME_2, x = long, y = lat, group = NAME_2),
                  size = 5) +
  geom_point(data = NULL, aes(x = 2.5712301, y = 49.1341839), shape = 23, size = 4, fill = "dodgerblue") + # asterix
  geom_point(data = NULL, aes(x = 2.7758079, y = 48.8722344), shape = 23, size = 4, fill = "indianred") + # disney
  coord_map() +
  labs(x = " ", y = " ") +
  geom_path(colour = 'black') +
  theme_bw() + scale_fill_brewer('Représentation visiteur', palette  = 'PuRd') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none')

print(p)

# save plot in high quality
png("idf_pa.png", width = 8, height = 14, units = 'in', res = 600)
p
dev.off()

# add image
p <- ggplot(data = departement_shape_df, mapping = aes(x = long, y = lat, group = group)) + # initialisation de la carte
  geom_path(colour = 'black') + # ajout du contour des départements
  coord_map() # coordonnées de carte pour garder échelle

img <-  png::readPNG(RCurl::getURLContent("http://www.icone-png.com/png/53/52955.png"))
img <- grid::rasterGrob(img, interpolate=TRUE)

library(cowplot)
plot <- ggdraw(p)
## tweak this to fit your plot
plot + draw_grob(grob = img, x = 2.5, width = 1, y = 49, height = 3)



# finally the map of Urb/Rur typology

caption <- "Classification: De Beer, J., Van Der Gaag, N., & Van Der Erf, R. (2014). New classification of urban and rural NUTS 2 regions in Europe. NIDI Working Papers, 2014/3. Retrieved from http://www.nidi.nl/shared/content/output/papers/nidi-wp-2014-03.pdf
\nIlya Kashnitsky (ikashnitsky.github.io)"

typ <-  basemap +

        geom_polygon(data = gghole(fort_map)[[1]],
                     aes(x=long, y=lat, group=group, fill=type),
                     color="grey30",size=.1)+
        geom_polygon(data = gghole(fort_map)[[2]],
                     aes(x=long, y=lat, group=group, fill=type),
                     color="grey30",size=.1)+
        scale_fill_viridis("NEUJOBS\nclassification of\nNUTS-2 regions",
                           option = "B", discrete = T, end = .8)+
        geom_path(data = bord, aes(x = long, y = lat, group = group),
                  color = "grey20",size = .5) +

        annotation_custom(grob = ggplotGrob(typ_mosaic),
                          xmin = 2500000, xmax = 4000000,
                          ymin = 4450000, ymax = 5450000)+
        annotation_custom(grob = ggplotGrob(an_sub),
                          xmin = 5400000, xmax = 6600000,
                          ymin = 2950000, ymax = 4150000)+
        labs(title = "Urban / Rural classification of NUTS-2 regions of Europe\n",
             caption = paste(strwrap(caption, width = 95), collapse = '\n'))+
        theme(plot.title = element_text(size = 20),
              plot.caption = element_text(size = 12))

ggsave(filename = "map.png", typ, width = 6.5, height = 8, dpi = 300)


