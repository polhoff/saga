
#CalcContributingArea
#DelineateWatershed
#FillSinks
#RasterToPolygon


CalcContributingArea <- function(ingrid = 'demFilled.sgrd', outgrid = 'carea')
	{

	print(paste('Input grid is : ' , ingrid, sep = ''))

	rsaga.topdown.processing(ingrid, out.carea = outgrid,  method="mfd", env = SAGA.env)



	print(paste('Output grid is : ' , outgrid, '.sgrd', sep = ''))


	out <- raster(paste(outgrid, ".sdat", sep = ''))
	
	print(paste('Returning object is : ' , outgrid, '.sdat', sep = ''))

	return(out)
	
	}

	
#CArea <- CalcContributingArea()
#proj4string(CArea) <- p_4



















































CalcSlope <- function(InputGrid = BaseGrid, outfile  = 'slope')
	{
	rsaga.slope(InputGrid, out.slope=outfile, method="maxslope", env = SAGA.env)

	out <- raster(paste(outfile, ".sdat", sep = ''))
	
	return(out)
	
	}


# cslope está en radianes, debe convertirse a grados
#spplot(slope, main = 'Slope')


#x <- CalcSlope()











































DelineateWatershed <- function(InputGrid = 'demFilled.sgrd', OutletPoint = field.outlet, outfile = 'basin')
	{

	stopifnot(class(OutletPoint) == 'SpatialPoints')
	
	p_4 <- proj4string(OutletPoint)
	
	#InputGrid <- 'demFilled.sgrd'
	#outfile <- 'basin'
	outfile.sgrd <- paste(outfile, '.sgrd' ,sep = '')
	outfile.shp <- paste(outfile, '.shp' ,sep = '')
	
	print(paste('Input grid is : ' , InputGrid))
	print(paste('Outfile name is  is : ' , outfile))
	
	xy_coords <- coordinates(OutletPoint)

	rsaga.geoprocessor(lib = 'ta_hydrology', 4,  param = list(TARGET_PT_X = xy_coords[,'x'], TARGET_PT_Y = xy_coords[,'y'], ELEVATION = 'demFilled.sgrd', AREA = outfile.sgrd, METHOD = 0))


	rsaga.geoprocessor(lib = 'shapes_grid', 6, param = list(GRID = outfile.sgrd, POLYGONS = outfile.shp, CLASS_ALL = 0, CLASS_ID = 100, SPLIT = 0))


	basin <- raster('basin.sdat')
	proj4string(basin) <- p_4
	
	basin.shp <- rasterToPolygons(basin, dissolve = TRUE)
	return(basin.shp)
	
	}



#basin.shp <- DelineateWatershed()





























DelineateWatershedBufferedOutlet <- function(InputGrid = 'demFilled', OutletPoint = field.outlet, outfile = 'basin', bufferSize = 5)
	{

	#InputGrid = 'demFilled'; OutletPoint = field.outlet; outfile = 'basin'; bufferSize = 5;
	
	InputGrid.sdat = paste(InputGrid, '.sdat', sep = '')
	InputGrid = paste(InputGrid, '.sgrd', sep = '')


	stopifnot(class(OutletPoint) == 'SpatialPoints')
	
	p_4 <- proj4string(OutletPoint)

	InputGrid.rst <- raster(InputGrid.sdat)
	proj4string(InputGrid.rst) <- p_4
	
	InputGrid.sp <- RasterToSpatialPoints(InputGrid.rst)
	
	BufferedOutlet <- gBuffer(OutletPoint, bufferSize, byid = FALSE)
	OutletPoints <- PointInPoly(InputGrid.sp, BufferedOutlet)


	DelineateWatershedBufferedOutlet
	stopifnot(class(OutletPoint) == 'SpatialPoints')
	
	p_4 <- proj4string(OutletPoint)
	
	#InputGrid <- 'demFilled.sgrd'
	#outfile <- 'basin'
	outfile.sgrd <- paste(outfile, '.sgrd' ,sep = '')
	outfile.shp <- paste(outfile, '.shp' ,sep = '')
	
	print(paste('Input grid is : ' , InputGrid))
	print(paste('Outfile name is  is : ' , outfile))
	
	xy_coords <- coordinates(OutletPoint)

	rsaga.geoprocessor(lib = 'ta_hydrology', 4,  param = list(TARGET_PT_X = xy_coords[,'x'], TARGET_PT_Y = xy_coords[,'y'], ELEVATION = 'demFilled.sgrd', AREA = outfile.sgrd, METHOD = 0))


	rsaga.geoprocessor(lib = 'shapes_grid', 6, param = list(GRID = outfile.sgrd, POLYGONS = outfile.shp, CLASS_ALL = 0, CLASS_ID = 100, SPLIT = 0))


	basin <- raster('basin.sdat')
	proj4string(basin) <- p_4
	
	basin.shp <- rasterToPolygons(basin, dissolve = TRUE)
	return(basin.shp)
	
	}



#basin.shp <- DelineateWatershed()
















































FillSinks <- function(InputGrid = BaseGrid, outfile = 'demFilled', n_method = 1, c_method = 'fill')
	{

	#Note:
	#rsaga.fill.sinks seems to work better than rsaga.sink.removal
	#BaseGrid is a character string of this form: 'SagaName.sgrd'
	stopifnot(c_method %in% c('fill','remove'))
	
	if(c_method == 'remove')
		{
		rsaga.sink.removal(in.dem = InputGrid, out.dem = outfile, method = n_method, env = SAGA.env)
		#rsaga.sink.removal(in.dem=BaseGrid, out.dem="demsink",method=1, env = SAGA.env)
		print('Method is rsaga.sink.removal')
		}
	
	if(c_method == 'fill')
		{
		rsaga.fill.sinks(InputGrid, outfile, method = "planchon.darboux.2001", out.flowdir = 'flowout', out.wshed = 'test')
		print('Method is rsaga.fill.sinks')
		}

	print(paste('Saved as : ' , outfile, '.sgrd', sep = ''))
	
	return(paste(outfile, '.sgrd', sep = ''))
	}


#FillSinks (BaseGrid)
#field.outlet <- FindOutlet(dem.c)


































































	


RasterToPolygon.Arc <- function(InputGrid, outfile = NA)
	{

	InputGrid.sgrd <- InputGrid
	
	if(is.na(outfile))
		{
		outfile <- GetObjectSelfName(InputGrid)
		outfile <- paste(outfile, '.shp', sep = '')
		}

	print('Output file is :', outfile)
	
	#InputGrid.sgrd = 'demFilled.sgrd'
	rsaga.geoprocessor(lib = 'shapes_grid', 6, param = list(GRID = InputGrid.sgrd,                     POLYGONS = outfile, CLASS_ALL = 0, CLASS_ID = 100, SPLIT = 0))

	}



#xx <- RasterToPolygon ('demFilled.sgrd')





















































RasterToPolygon <- function(InputGrid, outfile = NA)
	{

	#InputGrid = 'demFilled'; outfile = NA
	
	InputGrid.sgrd <- paste(InputGrid, '.sgrd', sep = '')
	
	if(is.na(outfile))
		{
		#outfile <- GetObjectSelfName(InputGrid)
		outfile <- paste(InputGrid, '.shp', sep = '')
		}
		else
		{outfile <- paste(outfile, '.shp', sep = '')}
	
	print(paste('Output file is :', outfile))
	
	#InputGrid.sgrd = 'demFilled.sgrd'
	rsaga.geoprocessor(lib = 'shapes_grid', 6, param = list(GRID = InputGrid.sgrd, POLYGONS = outfile, CLASS_ALL = 0, CLASS_ID = 100, SPLIT = 0))

	xx <- try(readOGR(paste(tempdir(), '/', outfile, sep = '')))
	return(xx)

	}



#xx1 <- RasterToPolygon ('demFilled', outfile = 'aa')


#rsaga.geoprocessor(lib = 'shapes_grid', 6, param = list(GRID = 'demFilled.sgrd', POLYGONS = 'a.shp', CLASS_ALL = 0, CLASS_ID = 100, SPLIT = 0))

































































































WetnessIndex <- function(InputGrid = BaseGrid, outfile = NA)
	{
	rsaga.wetness.index(InputGrid, outfile, env = SAGA.env)

	
	#inputNew <- paste(outfile, '.sgrd', sep = '')
	out <- raster(paste(outfile, '.sdat', sep = ''))
	#read 'test1.sgrd'
	#x1 <- read.sgrd(inputNew, env = SAGA.env)

	return(out)
	}

#WetIndex.2m <- readGDAL('test.asc')

#testTWI <- WetnessIndex(BaseGrid, outfile = 'apple')

