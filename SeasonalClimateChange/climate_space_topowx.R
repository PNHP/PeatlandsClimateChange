library(raster)
library(ecoclim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(weathermetrics)

#outdir <- "I:/projects/NCR/Workspace/auer/Visualization/climate_change_exposure_charts/climate_space_charts/temps/"
outdir <- "I:/projects/NCR/Workspace/auer/Visualization/climate_change_exposure_charts/climate_space_charts/climate_space_topwx/"

park_boundary <- readOGR("I:/projects/NCR/Data/Final/Boundaries", "NCR_region_wgs84")
parks <- readOGR("I:/projects/NCR/Data/Final/Boundaries", "NCR_All_Parks_buffer30km_wgs84")
study_area <- readOGR("I:/projects/NCR/Data/Final/Boundaries","NCR_Climate_Project_Area_WGS84")

##### Load climate normals for bio1 and bio12 #####
baseline <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means/")
future <- parseMetadata("I:/climate_data/ClimateNA/future/derived/RCP85_ensemble_2025/biovars")
d <- rbind(baseline, future)

#Colors
old_colors <- c("1948-1980" = "deepskyblue3","1981-2014" = "chartreuse3", "2011-2040"= "firebrick3")
pinks <- c("1948-1980" = "#fde0dd","1981-2014" = "#fa9fb5", "2011-2040"= "#c51b8a")
purples <- c("1948-1980" = "#e0ecf4","1981-2014" = "#9ebcda", "2011-2040"= "#8856a7")
#greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#1c9099", "2011-2040"= "#016c59") # original
greenblue <- c("1948-1980" = "#a8ddb5","1981-2014" = "#43a2ca", "2011-2040"= "#0868ac") # more contrast
#greens <- c("1948-1980" = "#a1d99b", "1981-2014" = "#31a354", "2011-2040"= "#006d2c")
greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#02818a", "2011-2040"= "#014636")
set1 <- c("1948-1980" = "#377eb8","1981-2014" = "#4daf4a", "2011-2040"= "#984ea3")
set2 <- c("1948-1980" = "#4daf4a","1981-2014" = "#984ea3", "2011-2040"= "#ff7f00")
set3 <- c("1948-1980" = "#4daf4a","1981-2014" = "#377eb8", "2011-2040"= "#e41a1c")
greens <- set1

for (p in unique(parks$code)){
  
  park <- parks[parks$code==p,]
  #p <- "NCR"
  #park <- park_boundary
  
  btemp <- raster(d$path[d$variable=="bio10" & d$year==1980])
  rtemp <- raster(d$path[d$variable=="bio10" & d$year==2014])
  ftemp <- raster(d$path[d$variable=="bio10" & d$year==2025])
  bppt <- raster(d$path[d$variable=="bio16" & d$year==1980])
  rppt <- raster(d$path[d$variable=="bio16" & d$year==2014])
  fppt <- raster(d$path[d$variable=="bio16" & d$year==2025])
  
  ### crop to park ###
  btemp <- crop(btemp, park)
  rtemp <- crop(rtemp, park)
  bppt <- crop(bppt, park)
  rppt <- crop(rppt, park)
  ftemp <- crop(ftemp, park)
  fppt <- crop(fppt, park)
  
  ### mask to park
  btemp <- mask(btemp, park)
  names(btemp) <- "temp"
  rtemp <- mask(rtemp, park)
  names(rtemp) <- "temp"
  bppt <- mask(bppt, park)
  names(bppt) <- "ppt"
  rppt <- mask(rppt, park)
  names(rppt) <- "ppt"
  ftemp <- mask(ftemp, park)
  names(ftemp) <- "temp"
  fppt <- mask(fppt, park)
  names(fppt) <- "ppt"
  
  #s <- stack(btemp, rtemp, ftemp, bppt, rppt, fppt)
  baseline <- stack(btemp, bppt)
  baseline <- as.data.frame(rasterToPoints(baseline))
  baseline$year <- "1948-1980"
  
  recent <- stack(rtemp, rppt)
  recent <- as.data.frame(rasterToPoints(recent))
  recent$year <- "1981-2014"
  
  future <- stack(ftemp, fppt)
  future <- as.data.frame(rasterToPoints(future))
  future$year <- "2011-2040"
  
  df <- rbind(baseline, recent, future)   ### should create a dataframe with rows for each raster
  
  #### same for winter variables
  Wbtemp <- raster(d$path[d$variable=="bio11" & d$year==1980])
  Wrtemp <- raster(d$path[d$variable=="bio11" & d$year==2014])
  Wftemp <- raster(d$path[d$variable=="bio11" & d$year==2025])
  Wbppt <- raster(d$path[d$variable=="bio17" & d$year==1980])
  Wrppt <- raster(d$path[d$variable=="bio17" & d$year==2014])
  Wfppt <- raster(d$path[d$variable=="bio17" & d$year==2025])
  
  ### crop to park ###
  Wbtemp <- crop(Wbtemp, park)
  Wrtemp <- crop(Wrtemp, park)
  Wbppt <- crop(Wbppt, park)
  Wrppt <- crop(Wrppt, park)
  Wftemp <- crop(Wftemp, park)
  Wfppt <- crop(Wfppt, park)
  
  ### mask to park
  Wbtemp <- mask(Wbtemp, park)
  names(Wbtemp) <- "temp"
  Wrtemp <- mask(Wrtemp, park)
  names(Wrtemp) <- "temp"
  Wbppt <- mask(Wbppt, park)
  names(Wbppt) <- "ppt"
  Wrppt <- mask(Wrppt, park)
  names(Wrppt) <- "ppt"
  Wftemp <- mask(Wftemp, park)
  names(Wftemp) <- "temp"
  Wfppt <- mask(Wfppt, park)
  names(Wfppt) <- "ppt"
  
  #s <- stack(btemp, rtemp, ftemp, bppt, rppt, fppt)
  Wbaseline <- stack(Wbtemp, Wbppt)
  Wbaseline <- as.data.frame(rasterToPoints(Wbaseline))
  Wbaseline$year <- "1948-1980"
  
  Wrecent <- stack(Wrtemp, Wrppt)
  Wrecent <- as.data.frame(rasterToPoints(Wrecent))
  Wrecent$year <- "1981-2014"
  
  Wfuture <- stack(Wftemp, Wfppt)
  Wfuture <- as.data.frame(rasterToPoints(Wfuture))
  Wfuture$year <- "2011-2040"
  
  Wdf <- rbind(Wbaseline, Wrecent, Wfuture)   
  #df$temp_F <- convert_temperature(df$temp, old_metric = "c", new_metric = "f")
  #df$ppt_in <- convert_precip(df$ppt, old_metric = "mm", new_metric = "inches")
  
  # p1 <- ggplot(data=df, aes(x=temp, y=ppt, color=year)) +
  #   geom_point(size=.2) + scale_color_manual(values = c("1948-1980" = "deepskyblue3","1981-2014" = "chartreuse3", "2011-2040"= "firebrick3"))  +
  #   scale_x_continuous(breaks=seq(floor(min(df$temp)), ceiling(max(df$temp)), 1)) +
  #   #scale_y_continuous(breaks=seq(floor(min(df$ppt)), ceiling(max(df$ppt)), 25)) +
  #   labs(x="Temp of Coldest Quarter (C)",
  #        y="Precip of Driest Quarter (mm)"
  #        ) +
  #   theme(#legend.position = c(.85, .93),
  #         legend.position = c(.21, .93),
  #         legend.title = element_blank(),
  #         legend.key.size = unit(.8, "lines"),
  #         legend.text = element_text(size =8),
  #         text= element_text(size=8))
  
  ############ Version with no legend  ###############
  
  p1 <- ggplot() +
    geom_point(data=df, aes(x=temp, y=ppt, color=year), size=.2) + 
    scale_color_manual(values = greens)  +
    scale_x_continuous(breaks=seq(floor(min(df$temp,na.rm = T)), ceiling(max(df$temp,na.rm = T)), 1)) +
    #scale_y_continuous(breaks=seq(floor(min(df$ppt)), ceiling(max(df$ppt)), 25)) +
#    labs(x="Temp of Coldest Quarter (C)",
#         y="Precip of Driest Quarter (mm)") +
    labs(x="Temp of Warmest Quarter (C)",
         y="Precip of Wettest Quarter (mm)") +
    
    theme(
      legend.position = "none",
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "gray82"),
      axis.text = element_text(colour = "gray42", size = 9),
      text= element_text(size=8)
    ) 
  
  ggsave(paste0(outdir, p, "_summer.png"), p1, width=1.25, height=1.25, units="in", scale=2)
  print(p1)
  dev.off()
  
  p2 <- ggplot() +
    geom_point(data=Wdf, aes(x=temp, y=ppt, color=year), size=.2) + 
    scale_color_manual(values = greens)  +
    scale_x_continuous(breaks=seq(floor(min(Wdf$temp,na.rm = T)), ceiling(max(Wdf$temp,na.rm = T)), 1)) +
    #scale_y_continuous(breaks=seq(floor(min(df$ppt)), ceiling(max(df$ppt)), 25)) +
        labs(x="Temp of Coldest Quarter (C)",
             y="Precip of Driest Quarter (mm)") +
    
    theme(
      legend.position = "none",
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "gray82"),
      axis.text = element_text(colour = "gray42", size = 9),
      text= element_text(size=8)
    ) 
  
  ggsave(paste0(outdir, p, "_winter.png"), p2, width=1.25, height=1.25, units="in", scale=2)
  print(p2)
  dev.off()
}  





#### F/in conversion charts ####
#   p2 <- ggplot() +
#   geom_point(data=df, aes(x=temp_F, y=ppt_in, color=year), size=.2) + 
#   scale_x_continuous(breaks=seq(floor(min(df$temp_F)), ceiling(max(df$temp_F)), 2)) +
# #  labs(x="Temp of Coldest Quarter (F)",
# #       y="Precip of Driest Quarter (in)") +
#     labs(x="Temp of Warmest Quarter (F)",
#          y="Precip of Wettest Quarter (in)") +
#   theme(
#       legend.position = "none",
#       panel.grid.major = element_line(colour = "white"),
#       panel.grid.minor = element_blank(),
#       panel.background = element_rect(fill = "gray82"),
#       axis.text = element_text(colour = "gray42", size = 9), 
#       text= element_text(size=8)
#     ) 


#p1 <- ggplotGrob(p1)
library(cowplot)
#library(grid)
#library(gridExtra)



png(paste0(outdir, p, "2.png"), width=2.5, height=2.5, units="in", res=1000)
ggdraw(switch_axis_position(p2, 'xy')) #+
#plot(p1) #+
#grid.draw(p2) +
#draw_plot(p1)

#grid.arrange(p1 , p2,
#             heights=c(1, 1),widths =c(1,1), top=p2, right=p2, as.table =TRUE)


dev.off()



