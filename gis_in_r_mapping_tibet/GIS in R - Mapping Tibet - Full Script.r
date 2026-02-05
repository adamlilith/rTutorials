### GIS OPERATIONS IN R: MAPPING TIBET - A TUTORIAL
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2026-02
###
### CONTENTS ###
### setup ###
### obtain data ###
### simple maps ###
### raster metadata ###
### vector metadata ###
###	raster math ###
###	"whole-raster" statistics ###
###	area calculations ###
### occurrences of Daphne bholua ###
###	slope, aspect, hill shade ###
### make nice plot! ###
### map using ggplot2 ###

#############
### setup ###
#############

# remove everything from memory -- reproducibility!
rm(list = ls())

library(terra) # GIS
library(enmSdmX) # GIS and SDMing
library(geodata) # spatial data
library(scales) # transparency and units of measurement
library(omnibus) # helper functions

# legends (and new kinds of plots)
# Need to install this from GitHub because it's not yet on CRAN:
devtools::install_github('adamlilith/legendary')
library(legendary)

# Other helpful packages (not used in this tutorial)
# sf: vector data (i.e., "shapefiles")
# stars: raster and vector time series data
# fasterRaster: GIS for large rasters/vectors

###################
### obtain data ###
###################

### country and province outlines

# NB: as of August 1st, the getData() function and the associated websites are not working, perhaps because of wildfires in California. So the two lines below may not work. If not, skip to the next two and change the path to the files on your computer.

# download from source (using low-resolution data for speed)
india_level_0 <- geodata::gadm(
	country = 'India',
	level = 0,
	resolution = 2,
	path = tempdir()
)

nepal_level_0 <- geodata::gadm(
	country = 'Nepal',
	level = 0,
	resolution = 2,
	path = tempdir()
)

nepal_level_1 <- geodata::gadm(
	country = 'Nepal',
	level = 1,
	resolution = 2,
	path = tempdir()
)

# # Alternative way to download these objects using GitHub
# url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/india_level_0.rds')
# india_level_0 <- readRDS(url)

# url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/nepal_level_0.rds')
# nepal_level_0 <- readRDS(url)

# url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/nepal_level_1.rds')
# nepal_level_1 <- readRDS(url)

# # If you had these saved to your computer and they were saved as GeoPackages:
# india_level_0 <- vect('C:/ecology/india_level_0.gpkg')
# nepal_level_0 <- vect('C:/ecology/nepal_level_0.gpkg')
# nepal_level_1 <- vect('C:/ecology/nepal_level_1.gpkg')

### download elevation raster

# download from source
elev <- geodata::elevation_30s(country = 'NPL', mask = FALSE, path = tempdir())

# # Alternative from GitHub
# url <- 'https://github.com/adamlilith/rTutorials/raw/master/data/nepal_elevation.tif'
# elev <- rast(url)

# If you had the raster saved to your computer:
# elev <- rast('C:/ecology/nepal_elevation.tif')

###################
### simple maps ###
###################

terra::plot(elev) # viridis color scale... not great for terrain

# terrain colors for elevation
terrain_colors <- terrain.colors(100)
terra::plot(elev, col = terrain_colors)

# force areas >5250 m to be white... better
snow <- elev >= 5250
snow[snow == 0] <- NA # force "not snow" to NA for plotting
terra::plot(elev, col = terrain_colors)
terra::plot(snow, col = 'white', add = TRUE)

# improve this by removing areas >5250 m (permanent snowline)
# so terrain color ramp "stretches" appropriately
elev_no_snow <- elev
elev_no_snow[elev_no_snow > 5250] <- NA
terra::plot(elev_no_snow, col = terrain_colors) # values >5250 not plotted (= white)
plot(nepal_level_0, add = TRUE) # max value on legend is misleading!

#######################
### raster metadata ###
#######################
	
elev
crs(elev) # coordinate reference system
cat(crs(elev)) # coordinate reference system (nicer printout)
res(elev) # resolution (in degrees, for this coordinate reference system)
names(elev) # names of the layer(s)

# spatial extent of the raster
terra::ext(elev) # extent
terra::as.vector(ext(elev)) # if you need the coordinates of the corners
xmin(elev)
xmax(elev)
ymin(elev)
ymax(elev)

# rows, columns, cells, and layers
nrow(elev)
ncol(elev)
ncell(elev)
nlyr(elev) # number of layers
	
#######################
### vector metadata ###
#######################

nepal_level_1
crs(elev) # coordinate reference system
cat(crs(elev)) # coordinate reference system (nicer printout)

# data.table-like properties
nrow(nepal_level_1)
ncol(nepal_level_1)
names(nepal_level_1)
dim(nepal_level_1)
nepal_level_1$VARNAME_1

# subsetting
sudur <- nepal_level_1[nepal_level_1$VARNAME_1 == 'Sudur Pashchimanchal']
sudur

plot(nepal_level_1)
plot(sudur, col = 'cornflowerblue', add = TRUE)

###################
###	raster math ###
###################

# All of these operations produce a new raster.

# single-raster math
elev_div <- elev / 1000
elevs <- c(elev, elev_div) # combine into a "stack" of rasters
plot(elevs)

elev_math <- elev + 10
elev_math <- elev - 10
elev_math <- elev * 10
elev_math <- elev^2
elev_math <- log10(elev)

# show areas with elevation > 1000 and < 2000 meters
elev_middle <- elev > 1000 & elev <= 2000
plot(elev_middle)

# replacing values
elev_replace <- elev
elev_replace[elev_replace > 1000 & elev_replace <= 2000] <- -5250
plot(elev_replace)

# raster-by-raster math
elev + elev
elev - elev
elev * elev
elev / elev

#################################
###	"whole-raster" statistics ###
#################################

# What if we want a single value from a raster (e.g., mean)? These are called "global" statistics.

# mean
stat <- mean(elev)
stat # gives you the same raster! :(

stat <- terra::global(elev, 'mean')
stat
class(stat) # it's data frame

# min/max
elev # notice the min/max values are shown
minmax(elev)

# quantiles
p <- c(0.05, 0.95)
stat <- global(elev, fun = quantile, probs = p, na.rm = TRUE)
stat

#########################
###	area calculations ###
#########################

# Calculate area of Nepal from vector and from raster.
# NB: Gemini says the area of Nepal is 147,181 km2.

# area of polygon of Nepal
terra::expanse(nepal_level_0) / 1000^2

# area of raster cells (but includes areas outside Nepal)
terra::expanse(elev) # sum of cell areas (in m2)
terra::expanse(elev) / 1000^2 # in km2

# area of raster cells that are in Nepal
elev_nepal <- terra::mask(elev, nepal_level_0)
plot(elev_nepal)
terra::expanse(elev_nepal) / 1000^2 # in km2

elev_mask <- mask(elev, nepal_level_0)
expanse(elev_mask) / 1000^2

####################################
### occurrences of Daphne bholua ###
####################################

### load data
# for Daphne bholua, courtesy of James Ojascastro Lucas!
# Please note that the coordinate have been rounded, and that I added some commonly-observed issues that were not in the data James sent me.  "Errors" are ones I introduced!!!

url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/daphne_bholua.csv')
occs <- read.csv(url)

# If they were on your computer:
# occs <- read.csv('C:/ecology/daphne_bholua.csv')
	
### clean occurrences
head(occs)

# plot... strange!
ll <- c('longitude', 'latitude')
plot(occs[ , ll])
plot(nepal_level_0, border = 'red', add = TRUE)

# look at coordinates
occs[  , ll]

# remove 0,0 coordinates (very common in GBIF data)
bads <- which(
	abs(occs$longitude - 0) < omnibus::eps() |
		abs(occs$latitude - 0) < omnibus::eps()
)

bads

nrow(occs)
occs <- occs[-bads, ]
nrow(occs)

# remove NA coordinates
bads <- which(
	is.na(occs$longitude) |
	is.na(occs$latitude)
)

bads

nrow(occs)
occs <- occs[-bads, ]
nrow(occs)

# correct record with negative latitude
plot(occs[, ll])

neg <- which(occs$latitude < 0)
occs$latitude[neg]
occs$latitude[neg] <- -1 * occs$latitude[neg]

# re-plot
plot(nepal_level_0, border='blue')
points(occs[ , ll])

### coordinate uncertainty
# examine coordinate uncertainties by constructing buffers around each point with radius equal to uncertainty

# convert occurrences to a SpatVector
wgs84 <- enmSdmX::getCRS('WGS84') # get CRS (see a list using `getCRS()`)
occs_vect <- terra::vect(occs, geom = ll, crs = wgs84)
occs_vect

plot(occs_vect)

# buffer points
occs_vect$coordUncertainty_meters
coord_uncer_buffers <- terra::buffer(
	occs_vect,
	width = occs_vect$coordUncertainty_meters
)

# plot radii reflecting uncertainty in coordinates
plot(coord_uncer_buffers)
points(occs_vect, pch = '.')
plot(nepal_level_0, add = TRUE)

# highlight records with no coordinate uncertainty
no_coord_uncer <- which(is.na(occs_vect$coordUncertainty_meters))
coord_uncer_na <- occs_vect[no_coord_uncer, ]
points(coord_uncer_na, pch = 16, cex = 0.3, col = 'red')

#################################
###	slope, aspect, hill shade ###
#################################

# Generate a "hillshade" raster for nice plotting. A "hillshade" raster creates "shadows" so it appears as if we were looking down from space.

# calculate slope and aspect
aspect <- terrain(elev, 'aspect', unit = 'radians')
slope <- terrain(elev, 'slope', unit = 'radians')

topo <- c(aspect, slope)
plot(topo)

# hillshade
hillshade <- shade(slope, aspect, direction = 180)

# a somewhat realistic plot
grays <- paste0('gray', 10:80)
plot(hillshade, col = grays)

#######################
### make nice plot! ###
#######################

# Make a nice plot with hillshading underneat to give 3D appearance.
plot(hillshade, col = grays, legend = FALSE)
plot(
	elev_no_snow,
	col = terrain_colors,
	alpha = 0.6,
	legend = FALSE,
	add = TRUE
)
plot(snow, col = 'white', alpha = 0.7, legend = FALSE, add = TRUE)
plot(occs_vect, pch = 1, add = TRUE)
plot(india_level_0, add = TRUE)
plot(nepal_level_1, add = TRUE)
contour(elev, levels = 5250, border = 'gray60', lwd = 0.4, add = TRUE)

## add a custom gradient legend

# get labels for elevation
elev_nepal <- mask(elev, nepal_level_0) # elevation just for Nepal
min_max_elev <- minmax(elev_nepal)
max_elev <- min_max_elev['max', 1]
labels <- seq(0, max_elev, length.out = 5)
labels <- omnibus::roundTo(labels, 100)

# hack for legend colors...
# adding lots of snow colors to the end to force
# "snow" to occur around 5250 m
snowwhite <- alpha('white', 0.7)
cols <- c(terrain_colors, rep(snowwhite, 90))

# from "legendary" package
legendGrad(
	'bottom',
	vert = FALSE,
	inset = 0.1,
	width = 1,
	height = 0.05,
	adjX = c(0, 1),

	col = cols,

	labels = labels,
	labAdj = -0.8,
	labPos = 1,
	labCex = 0.7,
	
	title = 'Elevation (m)',
	titleAdj = c(0.5, -1.1),
	titleCex = 0.8,
	
	boxBorder = NA
)

# add a scale bar
sbar(d = 100, xy = 'bottomleft', below = 'km', type = 'bar', adj = c(0.5, -1), cex = 0.8)

#########################
### map using ggplot2 ###
#########################

library(ggplot2) # graphics
library(ggspatial) # graphics for GIS
library(ggnewscale) # add new color/fill to a plot

# plot extent... set a bit larger than Nepal
extent <- terra::ext(elev)
extent <- as.vector(extent)

# contours
contour <- terra::as.contour(elev, levels = 5250)

map <- ggplot() +
	layer_spatial(hillshade) +
	scale_fill_gradient(low = 'gray30', high = 'gray100', guide = 'none') +
	ggnewscale::new_scale_fill() +
	layer_spatial(elev, alpha = 0.6) +
	scale_fill_gradientn(
		colors = terrain_colors,
		limits = c(0, 5250),
		guide = 'none'
	) +
	ggnewscale::new_scale_fill() +
	layer_spatial(snow) +
	scale_fill_manual(
		values = 'white',
		na.value = 'transparent',
		guide = 'none'
	) +
	layer_spatial(india_level_0, fill = NA, color = 'black', size = 0.3) +
	layer_spatial(nepal_level_1, fill = NA, color = 'black', size = 0.2) +
	layer_spatial(occs_vect, pch = 1, size = 1) +
	annotation_scale(location = 'bl', width_hint = 0.2, text_cex = 0.7) +
	coord_sf(
		xlim = extent[1:2],
		ylim = extent[3:4],
		expand = FALSE
	) +
	ggtitle(bquote('Known locations of ' * italic('Daphne bholua') * ' in Nepal'))

map

# # Save image (change path as needed)
# ggsave(map, filename = 'C:/ecology/tibet_with_daphne_bholua.png', dpi = 600, width = 8, height = 5, bg = 'white')

### DONE!
