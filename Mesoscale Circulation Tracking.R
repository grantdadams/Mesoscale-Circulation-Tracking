#This code takes geotiff SSH data in the form of a RData rasterstack and identifies areas of cyclonic and anti-cyclonic regions and there respestive border regions following the methodology of Domingues et al. (2016). The output is a rasterstack labelled "mesoscale.features.raster.stack" and a rasterstack of the geostrophic velocities labelled "geostrophic_velocity.RData". NOTE: you may wish to turn the raster into a factor, the Raster Attribute Table titled "rat" is at the end. Plotting is done at the end, alter m to alter the raster layer plotted.

# The features are labeled as: 1 = cyclonic region, 2 = cyclonic boundary region, 3 = common waters, 4 = anti-cyclonic boundary region, 5 = anti-cyclonic region.

#Domingues R, Goni G, Bringas F, et al (2016) Variability of preferred environmental conditions for Atlantic bluefin tuna (Thunnus thynnus) larvae in the Gulf of Mexico during 1993-2011. Fish Oceanogr 25:320–336. doi: 10.1111/fog.12152

# 6-15-2016 Grant Adams
# contact grantadams60091@gmail.com for questions



#Load SSH rasterstack data
ssh.recon.cropped <- file.choose() 

require(raster)
require(oce)

# NOTE: set the working directory to your desired save location.

##### Calculate SSH gradient
# Note - do not use the u and v components of the geostrphic velocity magnitude alone, the sign was not incorporated because we square it later.

delta.x <- res(ssh.recon.cropped[[1]])[1]*2 # the delta x calculated from the center of each raster cell
delta.y <- res(ssh.recon.cropped[[1]])[2]*2 # the delta y calculated from the center of each raster cell

# U-direction
lat.raster <- init(ssh.recon.cropped[[1]], "y") # Create a raster file where each cell value is the latitude
# U-direction
ssh.u.slope.subset <-  focal(ssh.recon.cropped[[1]], w = matrix(1/3, nrow = 3,ncol = 1), function(x){na.omit( x[1]-x[3]) }) # Calculate delta SSH for the u component
ssh.u.slope.subset <- mask(ssh.u.slope.subset, ssh.recon.cropped[[1]]) # Mask the file to the original study area
zonal.geostrophic.velocity <- overlay(ssh.u.slope.subset, lat.raster, fun=function(x,y){(x/delta.y)*(9.81/ coriolis(y)) }) # Calculate the u component of the geostrophic velocity magnitude by multiplying the u delta SSH by the gravitational constant over the coriollis parameter


# V-direction
ssh.v.slope.subset <-  focal(ssh.recon.cropped[[1]], w = matrix(1/3, nrow = 1,ncol = 3), function(x){na.omit( x[1]-x[3]) }) # Calculate delta SSH for the v component
ssh.v.slope.subset <- mask(ssh.v.slope.subset, ssh.recon.cropped[[1]]) # Mask
meridional.geostrophic.velocity <- overlay(ssh.v.slope.subset, lat.raster, fun=function(x,y){(x/delta.x)*9.81/(coriolis(y) ) }) # Calculate the v component of the geostrophic velocity magnitude by multiplying the v delta SSH by the gravitational constant over the coriollis parameter

geostrophic.velocity <- overlay(zonal.geostrophic.velocity,meridional.geostrophic.velocity, fun = function(x,y){sqrt(x^2+y^2)}) # calculate the geostrophic velocity from 


# We can now add subsequent geostrophic velocity layers from the layers in the SSH raster stack using a loop

for( i  in 2: dim(ssh.recon.cropped)[3]){
  ssh.u.slope.subset <-  focal(ssh.recon.cropped[[i]], w = matrix(1/3, nrow = 3,ncol = 1), function(x){na.omit( x[1]-x[3]) }) # Calculate delta SSH for the u component
  ssh.u.slope.subset <- mask(ssh.u.slope.subset, ssh.recon.cropped[[i]]) # Mask the file to the original study area
  zonal.geostrophic.velocity <- overlay(ssh.u.slope.subset, lat.raster, fun=function(x,y){(x/delta.y)*9.81/(coriolis(y)) }) # Calculate the u component of the geostrophic velocity magnitude by multiplying the u delta SSH by the gravitational constant over the coriollis parameter
  # V-direction
  ssh.v.slope.subset <-  focal(ssh.recon.cropped[[i]], w = matrix(1/3, nrow = 1,ncol = 3), function(x){na.omit( x[1]-x[3]) }) # Calculate delta SSH for the v component
  ssh.v.slope.subset <- mask(ssh.v.slope.subset, ssh.recon.cropped[[i]]) # Mask
  meridional.geostrophic.velocity <- overlay(ssh.v.slope.subset, lat.raster, fun=function(x,y){(x/delta.x)*9.81/(coriolis(y) ) }) # Calculate the v component of the geostrophic velocity magnitude by multiplying the v delta SSH by the gravitational constant over the coriollis parameter
  
  geostrophic.velocity.sub <- overlay(zonal.geostrophic.velocity,meridional.geostrophic.velocity, fun = function(x,y){sqrt(x^2+y^2)}) # calculate the geostrophic velocity for this layer
  geostrophic.velocity <- stack(geostrophic.velocity,geostrophic.velocity.sub) # add to the stack
  print(i)
}

save(geostrophic.velocity, file = "geostrophic_velocity.RData")


##### Characterize each raster 
# create empty raster stack
mesoscale.features.raster.stack <- ssh.recon.cropped
mesoscale.features.raster.stack[]<-NA
#characterize
for (i in 1:dim(mesoscale.features.raster.stack)[3]){ # 
  ssh.quantile.05 <- quantile(ssh.recon.cropped[[i]], probs = .05) # The 0.05 Quantile of the SSH layer
  ssh.quantile.15 <- quantile(ssh.recon.cropped[[i]], probs = .15) # The 0.15 Quantile of the SSH layer
  ssh.quantile.85 <- quantile(ssh.recon.cropped[[i]], probs = .85) # The 0.85 Quantile of the SSH layer
  ssh.quantile.95 <- quantile(ssh.recon.cropped[[i]], probs = .95) # The 0.95 Quantile of the SSH layer
  geostrophic.vel.quantile.50 <- quantile(geostrophic.velocity[[i]], probs = 0.5) # The 0.5 Quantile of the geostrophic velocity
  
  mesoscale.features.raster.stack[[i]] <- overlay(ssh.recon.cropped[[i]],geostrophic.velocity[[i]], fun = function(x,y){
    ifelse(x >  ssh.quantile.05 & x < ssh.quantile.15 & y > geostrophic.vel.quantile.50, 1, #cyclonic region test
           ifelse(x < ssh.quantile.15, 2, #cyclonic boundary test
                  ifelse(x>ssh.quantile.85 & x < ssh.quantile.95 & y > geostrophic.vel.quantile.50,4, # anti-cyclonic boundary region test
                         ifelse(x>ssh.quantile.85,5,3)))) #anti-cyclonic  region test
  })
  print(i)
  names(mesoscale.features.raster.stack)[[i]] <- paste(names(ssh.recon.cropped)[[i]],"mesoscale_features_characterized",sep="_")
}

# Convert the categories to factors
rat <- data.frame(ID = c(1,2,3,4,5))
rat$GOM_features <- c("CR","CB","CW","AB","AR")

save(mesoscale.features.raster.stack, file ="mesoscale_features_raster_stack_RData")

#####plot to double check
m=2501
par(mfrow= c(1,4))
x<-ssh.recon.cropped
x[[m]][2] <- - sort(c(abs(maxValue(x[[m]])), abs(minValue(x[[m]]))))[2]
x[[m]][1] <- sort(c(abs(maxValue(x[[m]])), abs(minValue(x[[m]]))))[2]
plot(x[[m]], main = "SSH")
plot(geostrophic.velocity[[m]], main = "Geostrophic velocity")
crs(x)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84") 
plot(terrain(x[[m]], opt = "slope", unit = "degrees"), main = "Slope (degrees)")
plot(mesoscale.features.raster.stack[[m]], main= "Mesoscale features")
legend(-95,15, legend = rat[,c(1)], bty = "n")
legend(-94,15, legend = rat[,c(2)], bty = "n")


