

## downloaded data with getSpatialData Package
## cloud mask and calculate Indices in Datacube format


gdalcubes_options(threads=8)

# list files for one year for creating an image collection
Sfiles = list.files(path = getwd(), recursive = TRUE, 
                    full.names = TRUE, pattern = "*.jp2") 

#create image collection, with Sentinel2 2A format
Sen20.col = create_image_collection(Sfiles, format = "Sentinel2_L2A")

# define Area of Interest
# 20mx20m resolution
vS = cube_view(srs = "EPSG:25832", extent = list(bottom = 5837771, top = 5844515, 
                                                 left = 453382, right = 463611,
                                                 t0= "2020-04-17", t1= "2020-10-01"), 
               dx = 20, dy = 20, dt = "P7D", resampling = "average",
               aggregation = "median")


## calculate Indices and one RGB Image as background picture for maps
# using SCL Layer for cloud masking

RGB.cube <- raster_cube(Sen20.col, vS, mask = image_mask("SCL", values = c(3,8,9,10)))
RGB.cube <-   select_bands(RGB.cube, c("B02","B03","B04"))
RGB.cube <-   plot(RGB.cube, zlim=c(0,1800), rgb=3:1)  
write_tif(RGB.cube, prefix = "RGB", dir = "YourPath")

# calculating NDVI
NDVI.cube <- raster_cube(Sen20.col, vS, mask = image_mask("SCL", values = c(3,8,9,10)))
NDVI.cube <-  select_bands(NDVI.cube, c("B04","B08"))
NDVI.cube <- apply_pixel(NDVI.cube, c("(B08-B04)/(B08+B04)"), names="NDVI")
plot(NDVI.cube, col=viridis, key.pos=1)
write_tif(NDVI.cube, prefix = "NDVI", dir = "YourPath")

# calculate NDMI
NDMI.cube <- raster_cube(Sen20.col, vS, mask = image_mask("SCL", values = c(3,8,9,10))) 
NDMI.cube <-  select_bands(NDMI.cube, c("B08", "B11")) 
NDMI.cube <-  apply_pixel(NDMI.cube, "(B08-B11)/(B08+B11)", names = "NDMI") 
plot(NDMI.cube, col=magma, key.pos=1, direction = -1) 
write_tif(NDMI.cube, prefix = "NDMI", dir = "YourPath")

