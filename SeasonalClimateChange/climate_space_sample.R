library(raster)
library(ecoclim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgdal)
library(weathermetrics)
library(sp)
library(here)
library(arcgisbinding)

arc.check_product()

outdir <- here("output") #I:/projects/NCR/Workspace/auer/Visualization/climate_change_exposure_charts/climate_space_charts/climate_space_sample_topowx/"

#get clip feature
bnd <- arc.open("E:/ClimateChange/Peatlands/PeatlandClimateChange/PeatlandClimateChange.gdb/Boundaries_20kmBuffer_Projec1")
bnd <- arc.select(bnd)
bnd <- arc.data2sp(bnd)


#get clip feature
parks <- arc.open("E:/ClimateChange/Peatlands/PeatlandClimateChange/PeatlandClimateChange.gdb/HIEL_AllSites_2019_Buffer")
parks <- arc.select(parks)
parks <- arc.data2sp(parks)

park_boundary <- readOGR("I:/projects/NCR/Data/Final/Boundaries", "NCR_region_wgs84")
parks <- readOGR("I:/projects/NCR/Data/Final/Boundaries", "NCR_All_Parks_buffer30km_wgs84")
study_area <- readOGR("I:/projects/NCR/Data/Final/Boundaries","NCR_Climate_Project_Area_WGS84")

# ##### Load climate normals for bio1 and bio12 #####
# baseline <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means/")
# future <- parseMetadata("I:/climate_data/ClimateNA/future/derived/RCP85_ensemble_2025/biovars")
# d <- rbind(baseline, future)

#Colors
old_colors <- c("1948-1980" = "deepskyblue3","1981-2014" = "chartreuse3", "2011-2040"= "firebrick3")
pinks <- c("1948-1980" = "#fde0dd","1981-2014" = "#fa9fb5", "2011-2040"= "#c51b8a")
purples <- c("1948-1980" = "#e0ecf4","1981-2014" = "#9ebcda", "2011-2040"= "#8856a7")
#greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#1c9099", "2011-2040"= "#016c59") # original
greenblue <- c("1948-1980" = "#a8ddb5","1981-2014" = "#43a2ca", "2011-2040"= "#0868ac") # more contrast
#greens <- c("1948-1980" = "#a1d99b", "1981-2014" = "#31a354", "2011-2040"= "#006d2c")
greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#02818a", "2011-2040"= "#014636")
set1 <- c("baseline" = "#377eb8","current" = "#4daf4a", "near-future"= "#984ea3")
set2 <- c("1948-1980" = "#4daf4a","1981-2014" = "#984ea3", "2011-2040"= "#ff7f00")
set3 <- c("1948-1980" = "#4daf4a","1981-2014" = "#377eb8", "2011-2040"= "#e41a1c")
greens <- set1

for(p in unique(parks$SITE)){
  park <- parks[parks$SITE==p,]
  #park <- bnd[bnd$Id==p,]
  #p <- "NCR"
  #park <- park_boundary
  
  ## create random sample of points within park boundary
  points <- spsample(park, n=50, "stratified")
 
  btemp <- raster(here("baseline","bio10.asc"))#d$path[d$variable=="bio10" & d$year==1980])
  rtemp <- raster(here("observed","bio10.asc"))#d$path[d$variable=="bio10" & d$year==2014])
  ftemp <- raster(here("nearfuture","bio10.asc"))#d$path[d$variable=="bio10" & d$year==2025])
  bppt <- raster(here("baseline","bio16.asc"))#d$path[d$variable=="bio16" & d$year==1980])
  rppt <- raster(here("observed","bio16.asc"))#d$path[d$variable=="bio16" & d$year==2014])
  fppt <- raster(here("nearfuture","bio16.asc"))#d$path[d$variable=="bio16" & d$year==2025])
   
  # btemp <- raster(d$path[d$variable=="bio10" & d$year==1980])
  # rtemp <- raster(d$path[d$variable=="bio10" & d$year==2014])
  # ftemp <- raster(d$path[d$variable=="bio10" & d$year==2025])
  # bppt <- raster(d$path[d$variable=="bio16" & d$year==1980])
  # rppt <- raster(d$path[d$variable=="bio16" & d$year==2014])
  # fppt <- raster(d$path[d$variable=="bio16" & d$year==2025])
  
  # extract to points
  btemp <- raster::extract(btemp, points, method='simple', df=TRUE)
  names(btemp) <- c("ID", "temp")
  btemp$year <- "baseline"
  rtemp <- raster::extract(rtemp, points, method='simple', df=TRUE)
  names(rtemp) <- c("ID", "temp")
  rtemp$year <- "current"
  bppt <- raster::extract(bppt, points, method='simple', df=TRUE)
  names(bppt) <- c("ID", "precip")
  bppt$year <- "baseline"
  rppt <- raster::extract(rppt, points, method='simple', df=TRUE)
  names(rppt) <- c("ID", "precip")
  rppt$year <- "current"
  ftemp <- raster::extract(ftemp, points, method='simple', df=TRUE)
  names(ftemp) <- c("ID", "temp")
  ftemp$year <- "near-future"
  fppt <- raster::extract(fppt, points, method='simple', df=TRUE)
  names(fppt) <- c("ID", "precip")
  fppt$year <- "near-future"
  
  dft <- rbind(btemp, rtemp, ftemp)
  dfp <- rbind(bppt, rppt,fppt)
  df <- merge(dft, dfp)
  df <- df[complete.cases(df),]

  ########### same for winter variables ##################
  Wbtemp <- raster(here("baseline","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==1980])
  Wrtemp <- raster(here("observed","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==2014])
  Wftemp <- raster(here("nearfuture","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==2025])
  Wbppt <- raster(here("baseline","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==1980])
  Wrppt <- raster(here("observed","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==2014])
  Wfppt <- raster(here("nearfuture","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==2025])
  
  Wbtemp <- raster::extract(Wbtemp, points, method='simple', df=TRUE)
  names(Wbtemp) <- c("ID", "temp")
  Wbtemp$year <- "baseline"
  Wrtemp <- raster::extract(Wrtemp, points, method='simple', df=TRUE)
  names(Wrtemp) <- c("ID", "temp")
  Wrtemp$year <- "current"
  Wbppt <- raster::extract(Wbppt, points, method='simple', df=TRUE)
  names(Wbppt) <- c("ID", "precip")
  Wbppt$year <- "baseline"
  Wrppt <- raster::extract(Wrppt, points, method='simple', df=TRUE)
  names(Wrppt) <- c("ID", "precip")
  Wrppt$year <- "current"
  Wftemp <- raster::extract(Wftemp, points, method='simple', df=TRUE)
  names(Wftemp) <- c("ID", "temp")
  Wftemp$year <- "near-future"
  Wfppt <- raster::extract(Wfppt, points, method='simple', df=TRUE)
  names(Wfppt) <- c("ID", "precip")
  Wfppt$year <- "near-future"
  
  Wdft <- rbind(Wbtemp, Wrtemp, Wftemp)
  Wdfp <- rbind(Wbppt, Wrppt,Wfppt)
  Wdf <- merge(Wdft, Wdfp)
  Wdf <- Wdf[complete.cases(Wdf),]
  
  ############ Version with no legend  ###############
  
  p1 <- ggplot() +
    geom_point(data=df, aes(x=temp, y=precip, color=year),  size=1) + 
    scale_color_manual(values=greens)  +
    scale_x_continuous(breaks=seq(floor(min(df$temp,na.rm = T)), ceiling(max(df$temp,na.rm = T)), 1)) +
    ggtitle("Summer") +
    theme(plot.title = element_text(size=6, margin=margin(4,0,0,0))) +
    labs(x="Temp of Warmest Quarter (C)",
         y="Precip of Wettest Quarter (mm)") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(0.02, 'cm'),
      panel.grid.major = element_line(colour="white"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill="gray82"),
      axis.text = element_text(colour="gray42", size=9),
      text = element_text(size=8)
    ) 
  
  p2 <- ggplot() +
    geom_point(data=Wdf, aes(x=temp, y=precip, color=year), size=1) + 
    scale_color_manual(values = greens)  +
    scale_x_continuous(breaks=seq(floor(min(Wdf$temp,na.rm = T)), ceiling(max(Wdf$temp,na.rm = T)), 1)) +
    ggtitle("Winter") +
    theme(plot.title = element_text(size=6, margin=margin(4,0,0,0))) +
    labs(x="Temp of Coldest Quarter (C)",
         y="Precip of Driest Quarter (mm)") +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(colour="white"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill="gray82"),
      axis.text = element_text(colour = "gray42", size = 9),
      text= element_text(size=8)
    ) 
 
  #extract legend
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(p1)  
   
  big_plot <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"), top=textGrob(p), nrow=1, ncol=2),
                           mylegend, nrow=2,heights=c(10, 1)) 
  
  ggsave(paste0(outdir, p, "_SeasonalChange.png"),  big_plot, width=2.5, height=1.25, units="in", scale=2)  
    dev.off()

    }  
