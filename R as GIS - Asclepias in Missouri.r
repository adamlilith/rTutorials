### R AS GIS - GEOGRAPHIC OPERATIONS AND MAKING MAPS
### Adam B. Smith | Missouri Botanical Garden | 2020-09
### This script demonstrates some common GIS operations in R. The end product is a nice-looking map of number of collected species and density of collections by county.
###
### source('C:/Ecology/Drive/R/rTutorials/R as GIS - Asclepias in Missouri.r')
###
### CONTENTS ###
### setup ###
### definitions ###
### get geographic data ###
### subset geographic data ###
### make hillshade raster ###
### enumerate number of collections in each county ###
### make maps of number of records in each county ###
### make maps of number of records per km2 in each county ###
### make maps of number of records per km2 in each county in equal-area projection ###
### enumerate number of species collected in each county ###
### put the plots together! ###
### extra: create leaflet maps ###

#############
### setup ###
#############

	# non-GIS packages
	library(gsheet) # if this does not work: install.packages('gsheet')... for opening Google Sheets
	library(scales) # if this does not work: install.packages('scales')... for transparency in plotting

	# GIS packages
	library(dismo) # if this does not work: install.packages('dismo')... species distribution modeling
	library(leaflet) # if this does not work: install.packages('leaflet')... creates web map
	library(raster) # if this does not work: install.packages('raster')... handles rasters
	library(rgeos) # if this does not work: install.packages('rgeos')... geographic operations on shapefiles
	library(sp) # if this does not work: install.packages('sp')... spatial library
	
	# Adam's packages... you will need to install these from GitHub
	# remotes::install_github('adamlilith/omnibus') # helper functions
	# remotes::install_github('adamlilith/statisfactory') # statistics
	# remotes::install_github('adamlilith/legendary') # plots and legends
	# remotes::install_github('adamlilith/enmSdm') # geographic tools and species distribution modeling

	library(legendary)
	library(enmSdm)

###################
### definitions ###
###################
	
	### definitions
	ll <- c('decimalLongitude', 'decimalLatitude')

	# Asclepias Google sheet URL: Note, you can go to this URL to see the worksheet
	url <- 'https://docs.google.com/spreadsheets/d/1JZ9souFVGVTV74IdiAfwsH5-D7JYFsUsqP6IQ7u1z5g/edit?usp=sharing'
	
###########################
### get geographic data ###
###########################
	
	# * getData() for state/county outlines
	# * getData() for elevation
	# * gsheet2tbl() for Asclepias data in a Google Sheet
	
	usa <- getData('GADM', country='USA', level=2)
	elev <- getData('worldclim', var='alt', res=2.5)
	
	asc <- gsheet2tbl(url)
	asc <- as.data.frame(asc)
	
##############################
### subset geographic data ###
##############################

	# * get shapefile for Missouri
	# * crop elevation to Missouri
	# * get just Asclepias with coordinates
	# * get just Asclepias and with "Missouri" in "stateProvince" field (may still miss some)
	# * convert Asclepias to a spatial points object
	# * plot and inspect

	mo <- usa[usa@data$NAME_1 == 'Missouri', ]
	elevMo <- crop(elev, mo)

	ascCoords <- asc[complete.cases(asc[ , ll]), ]
	ascCoordsMo <- ascCoords[ascCoords$stateProvince == 'Missouri', ]
	plot(ascCoordsMo[ , ll])

	ascSp <- SpatialPointsDataFrame(ascCoordsMo[ , ll], data=ascCoordsMo, proj4string=getCRS('wgs84', TRUE))

	plot(ascSp, pch=1)
	plot(elevMo, add=TRUE)
	plot(mo, add=TRUE)
	points(ascSp, pch=1)
	
#############################
### make hillshade raster ###
#############################

	# terrain() for slope and aspect
	# hillShade() for hillshade
	# mask to Missouri
	# plot in grayscale

	slope <- terrain(elevMo, 'slope')
	asp <- terrain(elevMo, 'aspect')
	
	hs <- hillShade(slope, asp)
	
	from <- minValue(hs)
	to <- maxValue(hs)
	breaks <- seq(from, to, length.out=102)
	
	plot(hs, col=paste0('gray', 0:100), breaks=breaks)
	
	mask <- rasterize(mo, elevMo)
	mask <- mask * 0 + 1
	hs <- hs * mask
	plot(hs, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE)
	
######################################################
### enumerate number of collections in each county ###
######################################################	

	# extract() to get county each record falls in
	# for loop for enumerating number of records in a county

	ascInMo <- extract(mo, ascSp)

	ascSp$countyFromGadm <- ascInMo$NAME_2
	
	mo$numRecs <- 0
	for (countRec in 1:nrow(ascSp)) {
	
		thisCounty <- which(mo@data$NAME_2 == ascInMo$NAME_2[countRec])
		mo@data$numRecs[thisCounty] <- mo@data$numRecs[thisCounty] + 1
	
	}
	
#####################################################
### make maps of number of records in each county ###
#####################################################	
	
	# rescale number of records to create color gradient
	# re-do in log scale because such disparities in number of collections
	
	# linear plot
	scales <- mo$numRecs / max(mo$numRecs)
	cols <- alpha('darkred', scales)
	plot(mo, col=cols)
	
	# log plot
	numRecsLog <- log10(mo$numRecs + 1)
	scales <- numRecsLog / max(numRecsLog)
	scales <- 0.8 * scales
	cols <- alpha('darkred', scales)

	plot(hs, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE)
	plot(mo, col=cols, add=TRUE)
	
#############################################################
### make maps of number of records per km2 in each county ###
#############################################################	

	# calculate county area:
	#	* project to equal-area
	#   * gArea(..., byid=TRUE) for each county's area

	moAlb <- sp::spTransform(mo, getCRS('albersNA', TRUE))
	moArea_km2 <- gArea(moAlb, byid=TRUE) / (1000 * 1000^2)
	
	# density of collections
	mo$numRecsPerThousandKm2 <- mo$numRecs / moArea_km2
	
	# linear plot
	scales <- mo$numRecsPerThousandKm2 / max(mo$numRecsPerThousandKm2)
	cols <- alpha('darkred', scales)
	plot(mo, col=cols)

	# log plot
	smallNum <- 0.5 * min(mo$numRecsPerThousandKm2[mo$numRecsPerThousandKm2 > 0])
	
	numRecsLog <- log10(mo$numRecsPerThousandKm2 + smallNum)
	numRecsLogRescale <- numRecsLog - min(numRecsLog)
	numRecsLogRescale <- numRecsLogRescale / max(numRecsLogRescale)
	numRecsLogRescale <- 0.8 * numRecsLogRescale
	
	cols <- alpha('yellow', numRecsLogRescale)

	plot(hs, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE)
	plot(mo, col=cols, add=TRUE)

######################################################################################
### make maps of number of records per km2 in each county in equal-area projection ###
######################################################################################

	# project hillshade raster to Albers equal-area projection
	# remove axes from plot (in first "plot()" line)
	# add gradient legend
	#		need to match color scale (which is on a log scale) to density of records

	hsAlb <- projectRaster(hs, crs=getCRS('albersNA'))
	
	plot(hsAlb, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE, ann=FALSE, box=FALSE, fg='white', col.axis='white')
	plot(moAlb, col=cols, add=TRUE, lwd=0.2)

	labels <- seq(min(numRecsLog), max(numRecsLog), length.out=5)
	labels <- 10^labels
	labels <- sprintf('%.1f', labels)
	legendary::legendGrad('right', inset=-0.15, width=0.2, height=1, labels=labels, col=c('white', 'yellow'), boxBorder=NA, title='Records\nper 1000 km2', labAdj=0.45, pos=4, titleAdj=c(0, 0.9))
	
	title(main='Density of georeferenced\nAsclepias records')
	
############################################################
### enumerate number of species collected in each county ###
############################################################

	# * look at species.. should we include them all?
	# * remove unwanted species
	# * enumerate number of species per county
	# * plot!
	
	sort(unique(ascSp$species))
	
	dim(ascSp)
	ascLegitSp <- ascSp[ascSp@data$species != '', ]
	dim(ascLegitSp)
	
	moAlb$numSpecies <- 0
	for (countCounty in 1:nrow(moAlb)) {
	
		thisCounty <- moAlb$NAME_2[countCounty]
		speciesInThisCounty <- ascLegitSp$species[ascLegitSp$countyFromGadm == thisCounty] # returns NAs, too!
		# recsInThisCounty <- ascLegitSp$species[ascLegitSp$countyFromGadm %==na% thisCounty] # from omnibus package... if you sue this, you don't need to do the next line
		speciesInThisCounty <- na.omit(speciesInThisCounty)
		numSpecies <- length(unique(speciesInThisCounty))
		moAlb$numSpecies[countCounty] <- numSpecies
	
	}
	
	### plot number of collected species per county
	scales <- moAlb$numSpecies / max(moAlb$numSpecies)
	cols <- alpha('red', scales)
	
	plot(hsAlb, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE, ann=FALSE, box=FALSE, fg='white', col.axis='white')
	plot(moAlb, col=cols, add=TRUE, lwd=0.2)

	labels <- seq(0, max(moAlb$numSpecies), length.out=5)
	legendGrad('right', inset=-0.15, width=0.2, height=1, labels=labels, col=c('white', 'red'), boxBorder=NA, title='Collected\nRichness', labAdj=0.45, pos=4, titleAdj=c(0, 0.9))
	
	title(main='Collection richness of\nAsclepias records')
	
	### explore relationships
	par(mfrow=c(1, 2))
	hist(moAlb$numSpecies / moAlb$numRecs, xlab='Number of species per record')
	plot(moAlb$numRecs / moAlb$numSpecies, xlab='Number of records', ylab='Number of species')
	
###############################
### put the plots together! ###
###############################	

	par(mfrow=c(1, 2))
	
	### plot collection density
	# log color scale
	smallNum <- 0.5 * min(mo$numRecsPerThousandKm2[mo$numRecsPerThousandKm2 > 0])
	
	numRecsLog <- log10(mo$numRecsPerThousandKm2 + smallNum)
	numRecsLogRescale <- numRecsLog - min(numRecsLog)
	numRecsLogRescale <- numRecsLogRescale / max(numRecsLogRescale)
	numRecsLogRescale <- 0.8 * numRecsLogRescale
	
	cols <- alpha('yellow', numRecsLogRescale)

	plot(hsAlb, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE, ann=FALSE, box=FALSE, fg='white', col.axis='white')
	plot(moAlb, col=cols, add=TRUE, lwd=0.2)

	labels <- seq(min(numRecsLog), max(numRecsLog), length.out=5)
	labels <- 10^labels
	labels <- sprintf('%.1f', labels)
	legendGrad('right', inset=-0.15, width=0.2, height=1, labels=labels, col=c('white', 'yellow'), boxBorder=NA, title='Records\nper 1000 km2', labAdj=0.45, pos=4, titleAdj=c(0, 0.9))
	
	title(main='Density of georeferenced\nAsclepias records')

	### plot number of collected species per county
	scales <- moAlb$numSpecies / max(moAlb$numSpecies)
	cols <- alpha('red', scales)
	
	plot(hsAlb, col=paste0('gray', 0:100), breaks=breaks, legend=FALSE, ann=FALSE, box=FALSE, fg='white', col.axis='white')
	plot(moAlb, col=cols, add=TRUE, lwd=0.2)

	labels <- seq(0, max(moAlb$numSpecies), length.out=5)
	legendGrad('right', inset=-0.15, width=0.2, height=1, labels=labels, col=c('white', 'red'), boxBorder=NA, title='Collected\nRichness', labAdj=0.45, pos=4, titleAdj=c(0, 0.9))
	
	title(main='Collection richness of\nAsclepias records')
	
##################################
### extra: create leaflet maps ###
##################################

	# each record by itself
	map <- leaflet()
	map <- addTiles(map) # by default adds OpenSteetMap basemap
	map <- addMarkers(map, lng=ascSp$decimalLongitude, lat=ascSp$decimalLatitude, popup=ascSp$species)
	map
	
	# records are clustered
	map <- leaflet()
	map <- addTiles(map) # by default adds OpenSteetMap basemap
	map <- addMarkers(map, lng=ascSp$decimalLongitude, lat=ascSp$decimalLatitude, popup=ascSp$species, clusterOptions=TRUE)
	map
	
	# records are clustered and with National Geographic basemap (more available at http://leaflet-extras.github.io/leaflet-providers/preview/... not all are free)
	map <- leaflet()
	map <- addTiles(map, urlTemplate='https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}')
	map <- addMarkers(map, lng=ascSp$decimalLongitude, lat=ascSp$decimalLatitude, popup=ascSp$species, clusterOptions=TRUE)
	map
	
##############################
omnibus::say('DONE!', level=1)
