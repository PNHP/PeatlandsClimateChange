library(raster)
library(ecoclim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)

outdir <- "I:/projects/NCR/Workspace/auer/Visualization/climate_change_exposure_charts/climate_space_charts/"

park_boundary <- readOGR("I:/projects/NCR/Data/Final/Boundaries", "NCR_region_wgs84")
parks <- readOGR("I:/projects/NCR/Data/Final/parks", "ncr_clim_all_park_bounds_wgs84")
study_area <- readOGR("I:/projects/NCR/Data/Final/Boundaries","NCR_Climate_Project_Area_WGS84")

park <- parks[parks$code=="PRWI",]

##### Load climate normals for bio1 and bio12 ##########
d <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means/")
btemp <- raster(d$path[d$variable=="bio1" & d$year==1980])
rtemp <- raster(d$path[d$variable=="bio1" & d$year==2014])
bppt <- raster(d$path[d$variable=="bio12" & d$year==1980])
rppt <- raster(d$path[d$variable=="bio12" & d$year==2014])

### mask to park ###
btemp <- crop(btemp, park)
rtemp <- crop(rtemp, park)
bppt <- crop(bppt, park)
rppt <- crop(rppt, park)

btemp <- mask(btemp, park)
names(btemp) <- "btemp"
rtemp <- mask(rtemp, park)
names(rtemp) <- "rtemp"
bppt <- mask(bppt, park)
names(bppt) <- "bppt"
rppt <- mask(rppt, park)
names(rppt) <- "rppt"

s <- stack(btemp, rtemp, bppt, rppt)
df <- as.data.frame(rasterToPoints(s))   ### should create a dataframe with rows for each raster

#prob <- c(.9)
#level <- contourLevel(df$btemp, df$bppt, prob=prob)
#level2 <- contourLevel(df$rtemp, df$rppt, prob=prob)

p <- ggplot() +
  stat_density2d(data=df, aes(x=btemp, y=bppt), geom="polygon", alpha=.2, fill="black") +
  stat_density2d(data=df, aes(x=rtemp, y=rppt), geom="polygon", alpha=.2, fill="red") +
  theme_minimal() +
  labs(x="annual average temperature (deg C)",
       y="annual total precipitation (mm)",
       title="Harper's Ferry")

# extract axis limits from first plot for use in other plots
# if the scatterplot is used, the range is too wide. if the basline contour is used, it's too narrow.
xrange <- ggplot_build(p)$panel$ranges[[1]]$x.range
yrange <- ggplot_build(p)$panel$ranges[[1]]$y.range
p <- p + xlim(xrange) + ylim(yrange)
ggsave(paste0(outdir, "/baseline_polygon_change_HAFE.png"), p, width=8, height=8)


p1 <- ggplot() +
  geom_point(data=df, aes(x=btemp, y=bppt), alpha=.7, color="black") +
  geom_point(data=df, aes(x=rtemp, y=rppt), alpha=.7, color="red") +
  theme_minimal() +
  labs(x="annual average temperature (deg C)",
       y="annual total precipitation (mm)",
       title="Prince William")
ggsave(paste0(outdir, "/baseline_point_change_PRWI.png"), p1, width=8, height=8)

