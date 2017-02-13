# Mesoscale-Circulation-Tracking


##This code takes geotiff SSH data in the form of a RData rasterstack and identifies areas of cyclonic and anti-cyclonic regions and their respestive border regions following the methodology of Domingues et al. (2016). The output is a rasterstack labelled "mesoscale.features.raster.stack" and a rasterstack of the geostrophic velocities labelled "geostrophic_velocity.RData". 

##NOTE: you may wish to turn the raster into a factor, the Raster Attribute Table titled "rat" is at the end. Plotting is done at the end, alter m to alter the raster layer plotted.

## The features are labeled as: 1 = cyclonic region, 2 = cyclonic boundary region, 3 = common waters, 4 = anti-cyclonic boundary region, 5 = anti-cyclonic region.

##Domingues R, Goni G, Bringas F, et al (2016) Variability of preferred environmental conditions for Atlantic bluefin tuna (Thunnus thynnus) larvae in the Gulf of Mexico during 1993-2011. Fish Oceanogr 25:320–336. doi: 10.1111/fog.12152




load("Mesoscale Circulation Examples.RData")
par(mfrow= c(1,4))
plot(x, main = "SSH")
plot(geostrophic.velocity.example, main = "Geostrophic velocity")
crs(x)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84") 
plot(terrain(x, opt = "slope", unit = "degrees"), main = "Slope (degrees)")
plot(mesoscale.features.raster.example, main= "Mesoscale features")
legend(-95,15, legend = rat[,c(1)], bty = "n")
legend(-94,15, legend = rat[,c(2)], bty = "n")
