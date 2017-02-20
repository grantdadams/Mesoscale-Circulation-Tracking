# This function takes Mean Dynamic Topography (MDT) data and adds it to Sea Surface Height Anomaly (SSHA) data to get sea surface height (SSH). The key point of this function is that it alters the MDT data extent and resolution to that of the SSHA to then be added together. The function also crops the data to the desired study area. 
#The input requires a geotifs for the MDT and SSHA data. Please be careful with the units of both data sets as the current code accounts for MDT as meters and SSHA as cm. CRS of both data are assumed to be the same.
# The output is a raster stack names "ssh.stack" and it will save in same folder as the MDT data.
# Plotting function is at the end
# 6-10-2016 for questions contact grant.adams@eagles.usm.edu

##### Data inputs #######

# Define the study area
latitude.max <- 20   # Max latitude
latitude.min <- -33   # Min latitude
longitude.max <- -84  # Max longitude
longitude.min <- -95  # Min longitude
  
# Project directory
proj_wd <-getwd()

# Select the folder where the raster SSHA data are  
ssha.raster.wd <- choose.dir(, caption = "Where are the SSHA raster files to analyze?")

# Select the folder where the raster MDT data are  
mdt.raster.wd <- choose.dir(, caption = "Where is the MDT raster file to analyze?")

require(raster)


###### Analysis

# Create a polygon to cut the rasters later
raster.men.grid=raster(xmn=min(longitude.min), xmx=max(longitude.max), ymn=min(latitude.min), ymx=max(latitude.max), resolution =1/6, crs = NA)
poly<- rasterToPolygons(raster.men.grid, na.rm=F)


### Load and process SSHA data
setwd(ssha.raster.wd)
ssha.raster_data=list.files(ssha.raster.wd, pattern= ".tif")
ssha.stack<- stack(ssha.raster_data)
ssha.stack<- crop(ssha.stack, extent(poly))


#Load and process MDT data
setwd(mdt.raster.wd)
mdt.raster_data=list.files(mdt.raster.wd, pattern= ".tif")
ssh.mdt <- raster(mdt.raster_data)
ssh.mdt<- crop(ssh.mdt, extent(poly))


# Convert the MDT values to a raster with the same resolution and extent as the SSHA data
empty.raster <- ssha.stack[[1]] 
empty.raster[] <- NA # create an empty raster from the SSHA raster
empty.shape <- rasterToPolygons(empty.raster,na.rm=F) # Transform it to a shape file
list.dat <- vector(mode="list", length = length(empty.shape@polygons)) # Create a list to fill with the boundaries of each raster/shape cell from the SSHA data
for ( i  in 1:length(empty.shape@polygons)){
  list.dat[[i]] <- empty.shape@polygons[[i]]@Polygons[[1]]@coords # fill the list with the boundaries of the SSHA raster/shape cells
  ext <- SpatialPolygons(list(Polygons(list(Polygon(list.dat[[i]])),ID = as.character(i)))) # Create spatial polygon from the raster cell boundaries
  empty.raster[i] <- extract(ssh.mdt,ext,fun=mean, method="simple", na.rm=T, weights=TRUE) # fill the empty raster with the weighted mean of the MDT data
}
empty.raster[empty.raster%in%c(0)] <-NA # Make zero data NA. The previous function will will in NA data with 0s.
MDT_at_SSHA_resolution<-empty.raster # Change name


##### Add SSHA data to MDT data and adjust units of all to meters
ssh.stack <- overlay(ssha.stack, MDT_at_SSHA_resolution, fun=function(x,y){return(x*0.01+y)}) # SSHA is in cm and 
names(ssh.stack) <- paste(names(ssha.stack),"SSH_data",sep = "_")

#### Plot to make sure it works
m=1
par(mfrow=c(1,3))
plot(ssha.stack[[m]])
plot(mean_dynamic_topography_0.5_degree_resolution)
plot(ssh.stack[[m]])

# Save
setwd(proj_wd)
save(ssh.stack, file = "ssh_stack.RData")








