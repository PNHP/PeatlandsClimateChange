library(dismo)
library(arcgisbinding)
library(raster)
library(here)

arc.check_product()


#get clip feature
bnd <- arc.open("E:/ClimateChange/Peatlands/PeatlandClimateChange/PeatlandClimateChange.gdb/Boundaries_20kmBuffer_Projec1")
bnd <- arc.select(bnd)
bnd <- arc.data2sp(bnd)




############################################################
# precip
setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII"))

raslist <- list.files(pattern="PPT")
gridlist <- as.list(raslist)

nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"),"/", names(gridlist[i]), ".asc", sep="")
  a <- crop(ras,bnd)
  writeRaster(a, filename = fn, format = "ascii", overwrite = TRUE) 
}

setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"))
raslist <- list.files(pattern="PPT")
gridlist <- as.list(raslist)

# create a stack
prc <- stack(gridlist)

############################################################
# tmin
setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII"))
raslist <- list.files(pattern="Tmin")
gridlist <- as.list(raslist)

nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"),"/", names(gridlist[i]), ".asc", sep="")
  a <- crop(ras,bnd)
  writeRaster(a, filename = fn, format = "ascii", overwrite = TRUE) # datatype = "INT4S"
}

setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"))
raslist <- list.files(pattern="Tmin")
gridlist <- as.list(raslist)
tmn <- stack(gridlist)

############################################################
# tmax
setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII"))
# create a stack
raslist <- list.files(pattern="Tmax")
gridlist <- as.list(raslist)

nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"),"/", names(gridlist[i]), ".asc", sep="")
  a <- crop(ras,bnd)
  writeRaster(a, filename = fn, format = "ascii", overwrite = TRUE) # datatype = "INT4S"
}

setwd(here("nearfuture","NA_ENSEMBLE_rcp45_2020s_Monthly_ASCII_clip"))
raslist <- list.files(pattern="Tmax")
gridlist <- as.list(raslist)
tmx <- stack(gridlist)





# 
b <- biovars(prc, tmn, tmx)

as.matrix(b)


setwd(here("nearfuture"))
writeRaster(b, names(b), bylayer=TRUE, format='ascii')


