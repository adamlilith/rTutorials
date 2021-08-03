# colors for topography and snow
terrainCols <- terrain.colors(100, alpha=0.6)
snowwhite <- alpha('white', 0.6)

# snow layer for this province
snowProv <- elevProv >= 5000

# plot!
plot(hillshade, col=grays, axes=FALSE, legend=FALSE)
plot(elevProv, col=terrainCols, range=c(0, 5000), legend=FALSE, axes=FALSE, add=TRUE)
plot(snowProv, col=c(NA, snowwhite), axes=FALSE, legend=FALSE, add=TRUE)
plot(nepal1focus, add=TRUE)
plot(box, add=TRUE)

## add a custom gradient legend

# get labels for elevation
maxElev <- global(elevProv, 'max')
maxElev <- maxElev$max
labels <- pretty(c(0, maxElev))

# hack for legend colors...
# adding lots of snow colors to the end to force
# "snow" to occur around 5000 m
cols <- c(terrainCols, rep(snowwhite, 60))

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
# I got the x/y coordinates by using "click(hillshade, n=1, xy=TRUE)"
# then a lot of trial/error
xy <- c(85.72585, 28.50584) # left, center of scale bar
sbar(100, xy=xy, below='kilometers', type='bar', adj=c(0.5, -1), cex=0.8)

