### RASTER OPERATIONS IN R: A TUTORIAL
### Adam B. Smith
### Missouri Botanical Garden
### adam.smith@mobot.org
### 2021-08
###
### CONTENTS ###
### setup ###
### obtain data ###
### raster operations ###
### simple(?) plot ###
###	raster metadata ###
###	raster math ###
###	"whole-raster" statistics ###
###	area ###
###	slope, aspect, hill shade ###
### occurrences (optional) ###
### make nice plot! ###

#############
### setup ###
#############

# remove everything from memory -- reproducibility!
rm(list=ls())

# load libraries
# Note that this section installs the packages
# if they are not yet on your system.

# raster package: old package for rasters
if (!require(raster)) {
	install.packages('raster')
	library(raster)
}

# terra package: new package for rasters
if (!require(terra)) {
	install.packages('terra')
	library(terra)
}

# scales package: has function alpha() for transparent colors
if (!require(scales)) {
	install.packages('scales')
	library(scales)
}

# legendary package: legends (and new kinds of plots)
if (!require(legendary)) {
	devtools::install_github('adamlilith/legendary')
	library(legendary)
	# NB: If this doesn't work you can download the latest version of the zip/tar file from https://github.com/adamlilith/legendary/tree/master/zipTarFiles and install it manually in R.
}

# other helpful packages (not used in this tutorial)

# sp: old package for vector data ("shapefiles")

# sf: new package for vector data ("shapefiles")

# rgeos: spatial operations for objects in sp package

# geosphere: spatial operations for objects in sp package

###################
### obtain data ###
###################

### country and province outlines

# NB: as of August 1st, the getData() function and the associated websites are not working, perhaps because of wildfires in California. So the two lines below may not work. If not, skip to the next two and change the path to the files on your computer.

# download from source
nepal0 <- getData('GADM', country='NPL', level=0)
nepal1 <- getData('GADM', country='NPL', level=1)

# alternative #1: from GitHub
url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/nepal0.rda')
load(url)
url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/nepal1.rda')
load(url)

# alternative #2: from disk
# change the path name accordingly
load('E:/Ecology/Drive/R/rTutorials/data/nepal0.rda')
load('E:/Ecology/Drive/R/rTutorials/data/nepal1.rda')

# convert to SpatVector (terra) format
nepal0 <- vect(nepal0)
nepal1 <- vect(nepal1)

### download elevation raster

# download from source
elev <- getData('alt', country='NPL', mask=FALSE)
elev <- rast(elev)

# alternative #1: from GitHub
url <- 'https://github.com/adamlilith/rTutorials/raw/master/data/nepal_elevation.tif'
elev <- rast(url)

# alternative #2: from your computer
elev <- rast('E:/Ecology/Drive/R/rTutorials/data/nepal_elevation.tif')
	
######################
### simple(?) plot ###
######################

# plain plot... colors are backwards!
plot(elev)

# breaks plot... yes, backwards!
plot(elev, breaks=10)

# re-order colors
cols <- terrain.colors(100)
plot(elev, col=cols)

# force areas >5000 m to be white... better
snow <- elev > 5000
plot(elev, col=cols)
plot(snow, col=c(NA, 'white'), add=TRUE)

# improve this by plotting areas <5000 and >=5000 m separately
plot(elev, col=cols, range=c(0, 5000))
plot(snow, col=c(NA, 'white'), add=TRUE)

#######################
### raster metadata ###
#######################

elev # metadata

# assigning a coordinate reference system
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs'

crs(elev) <- wgs84

## how to get this metadata
# coordinate reference system
crs(elev)

# extent
ext(elev)
xmin(elev)
ymin(elev)

# resolution (cell size)
res(elev)

# names
names(elev)

# rows, columns, cells, and layers
nrow(elev)
ncol(elev)
ncell(elev)
nlyr(elev)

?terra

###################
###	raster math ###
###################

# All of these operations produce a new raster.

# single-raster math
new <- elev + 1000
new <- elev - 1000
new <- elev / 1000
new <- elev * 1000
new <- elev^2
new <- sqrt(elev)

# logical operations
new <- elev > 1000 & elev < 3000
plot(new)

new <- elev == 50
plot(new)

# raster-by-raster math
new <- elev + elev
plot(new)

# to make two rasters congruent
elev1 <- project(elev1, elev) # CRS
elev1 <- resample(elev1, elev) # cell size
elev1 <- crop(elev1, elev) # overlap

#################################
###	"whole-raster" statistics ###
#################################

# What if we want a single value from a raster (e.g., mean)? These are called "global" statistics.

elev # notice the min/max values are shown

# mean
stat <- global(elev, 'mean')
stat
class(stat)
stat$mean

# min/max
global(elev, 'min')
global(elev, 'max')
global(elev, median)

# quantiles
global(elev, quantile, prob=0.95)

############
###	area ###
############

# Calculate area of Nepal from shape and from raster.

# NB: area of Nepal is 147,516 km2

# area of raster cells
expanse(elev) / 1000^2

plot(elev)
plot(nepal0, add=TRUE)

# area of polygon
expanse(nepal0) / 1000^2

# area of raster cells that touch the polygon
nepalRast <- rasterize(nepal0, elev)
plot(nepalRast)
expanse(nepalRast) / 1000^2

#################################
###	slope, aspect, hill shade ###
#################################

# Generate a "hillshade" raster for nice plotting.

# These plotting operations can take a while, so let's just focus on a single province of Nepal.

### generate hillshade raster

# manually select a province
plot(nepal1)
where <- click(nepal1, n=1)
where

prov <- nepal1[nepal1$VARNAME_1 == 'Madhyamanchal', ]

plot(prov)

# enlarge focal area for a nicer plot
buff <- buffer(prov, 20000)

plot(buff)
plot(prov, col='blue', add=TRUE)

# crop elevation raster to this province
elevProv <- crop(elev, buff)

plot(elevProv)
plot(prov, add=TRUE)

# slope and aspect
slope <- terrain(elev, 'slope', unit='radians')
aspect <- terrain(elev, 'aspect', unit='radians')

topo <- c(slope, aspect)
plot(topo)

# hillshade
hs <- shade(slope, aspect, direction=180)

# an OK plot
grays <- paste('gray', 100:30)
plot(hs, col=grays)

###################
### occurrences ###
###################

### load data
# for Daphne bholua, courtesy of James Lucas!
# Please note that the coordinate have been rounded, and that I added some commonly-observed issues that were not in the data James sent me.

url <- url('https://github.com/adamlilith/rTutorials/raw/master/data/daphne_bholua.csv')
occs <- read.csv(url)

# alternative #1: from your computer
occs <- read.csv('E:/Ecology/Drive/R/rTutorials/data/daphne_bholua.csv')
	
### clean occurrences
head(occs)

# plot... strange!
ll <- c('longitude', 'latitude')
plot(occs[ , ll])
plot(nepal0, border='blue', add=TRUE)

# look at coordinates
occs[  , ll]

# remove 0,0 coordinates
bads <- which(
	occs$longitude == 0 |
	occs$latitude == 0
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
neg <- which(occs$latitude < 0)
occs$latitude[neg]
occs$latitude[neg] <- -1 * occs$latitude[neg]

# re-plot
plot(nepal0, border='blue')
points(occs[ , ll])

### zoom
# lets you interactively zoom into a place
zoom(nepal0)
points(occs[ , ll])

### coordinate uncertainty
# examine coordinate uncertainties by constructing buffers around each point with radius equal to uncertainty

# convert occurrences to a SpatVector
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs'
longLat <- as.matrix(occs[ , ll])
occsVect <- vect(longLat, 'points', atts=occs, crs=wgs84)
occsVect

# buffer
occsVect$coordUncertainty_meters
cu <- buffer(occsVect, occsVect$coordUncertainty_meters)

# plot
plot(cu)
points(occsVect, pch='.')
plot(nepal0, add=TRUE)

# highlight records with no CU
cuIsNa <- which(is.na(occsVect$coordUncertainty_meters))
occsCuNa <- occsVect[cuIsNa, ]
points(occsCuNa, pch=16, cex=0.3, col='red')

#######################
### make nice plot! ###
#######################

# Make a nice plot with hillshading. This can be slow, so we will plot just once province.

prov <- nepal1[nepal1$VARNAME_1 == 'Madhyamanchal', ]
plot(prov)

# expand plotting region a bit to provide context
provBuff <- buffer(prov, 20000)

# crop elevation raster to the bounding box
elevProv <- crop(elev, provBuff)

# crop provinces to plotting region
extent <- ext(elevProv)
nepal1focus <- crop(nepal1, extent)

# colors for topography and snow
terrainCols <- terrain.colors(100, alpha=0.6)
snowwhite <- alpha('white', 0.6)

# snow layer for this province
snowProv <- elevProv >= 5000

# slope and aspect
aspect <- terrain(elevProv, 'aspect', unit='radians')
slope <- terrain(elevProv, 'slope', unit='radians')

topo <- c(aspect, slope)
plot(topo)

# hillshade
hillshade <- shade(slope, aspect, direction=180)

# an OK plot
grays <- paste0('gray', 100:20)
plot(hillshade, col=grays)

# nice plot!
plot(hillshade, col=grays, axes=FALSE, legend=FALSE)
plot(elevProv, col=terrainCols, range=c(0, 5000), legend=FALSE, axes=FALSE, add=TRUE)
plot(snowProv, col=c(NA, snowwhite), axes=FALSE, legend=FALSE, add=TRUE)
plot(nepal1focus, add=TRUE)

# add contour lines at 5000 m
contour(elevProv, levels=5000, border='gray', add=TRUE)

## add a custom gradient legend

# get labels for elevation
maxElev <- global(elevProv, 'max')
maxElev <- maxElev$max
labels <- pretty(c(0, maxElev))

# hack for legend colors...
# adding lots of snow colors to the end to force
# "snow" to occur around 5000 m
cols <- c(terrainCols, rep(snowwhite, 90))

# from "legendary" package
legendGrad(
	'bottom',
	vert=FALSE,
	inset=0.05,
	width=1,
	height=0.05,
	adjX=c(0, 1),

	col=cols,

	labels=labels,
	labAdj=-0.8,
	labPos=1,
	labCex=0.7,
	
	title='Elevation (m)',
	titleAdj=c(0.5, -1.1),
	titleCex=0.8,
	
	boxBorder=NA
)

## add scale bar
# We need to get the x/y position of the left side of the scale bar. We can use click(), but here we'll do it automatically based on the corners of the plot region. This code places it in the bottom left of the map.
x <- corners[1] + 0.03 * (corners[2] - corners[1])
y <- corners[3] + 0.04 * (corners[4] - corners[3])

xy <- c(x, y) # left, center of scale bar
sbar(100, xy=xy, below='kilometers', type='bar', adj=c(0.5, -1), cex=0.8)

## add occurrences
occsFocal <- crop(occsVect, box)
points(occsFocal)

