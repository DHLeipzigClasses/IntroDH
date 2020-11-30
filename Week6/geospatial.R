library(tidyverse)
library(maps)
library(PBSmapping)
library(mapproj)
# library(gganimate)
library(mapview)

xlim <- c(-12,55)
ylim <- c(20,60)

grep("grey", colours(), value = T)
landborder <- "lightgrey"
land <- "lightgrey"
worldmap <- map_data("world")

# for clipping with PBSmapping
colnames(worldmap) <- c("X","Y","PID","POS","region","subregion")
worldmap <- clipPolys(worldmap, xlim = xlim, ylim = ylim, keepExtra=TRUE)


locsRaw <- read_csv("http://atlantides.org/downloads/pleiades/dumps/pleiades-places-latest.csv.gz")
View(locsRaw)
dataLabel <- "Data: Pleiades Project"

# periods and features ####

periods <- rbind(
  c("archaic","750-550BCE"),
  c("classical","550-330BCE"),
  c("hellenistic-republican","330-30BCE"),
  c("roman","30BC-300CE"),
  c("late-antique","300-640CE")
) %>% as_tibble

features <- rbind(
  c("","locations"),
  c("settlement","settlements"),
  c("fort","forts"),
  c("temple","temples"),
  c("villa","villas"),
  c("station","stations"),
  c("theatre","theatres"),
  c("amphitheatre","amphitheatres"),
  c("church","churches"),
  c("bridge","bridges"),
  c("bath","baths"),
  c("cemetery","cemeteries"),
  c("plaza","plazas"),
  c("arch","archs")
) %>% as_tibble

locPleiades <- geom_point(
  data = locsRaw,
  aes(reprLong, reprLat),
  color = "grey70",
  alpha = .75,
  size = 1)

# loops ####

for (i in seq_along(features$V1)) {
  locs <- filter(locsRaw, grepl(features$V1[i],featureTypes))
  for (ii in seq_along(periods$V2)) {
    locPer <- filter(locs, grepl(periods$V1[ii],timePeriodsKeys))
    fName <- paste0("Pleiades_",features$V2[i],sprintf("%02d",ii),".png")
    header <- paste0(features$V2[i]," in the ",periods$V1[ii]," period (",periods$V2[ii],")")
    p <- ggplot(locPer) +
      coord_map(xlim = xlim, ylim = ylim) +
      geom_polygon(data = worldmap, 
                   mapping = aes(X,Y,group=PID), 
                   size = 0.1, 
                   colour = landborder,
                   fill = land,
                   alpha = 1) +
      locPleiades + 
      geom_point(aes(y=reprLat,x=reprLong),
                 color="salmon",
                 alpha=.75,
                 size=1) + 
      labs(title = header, y="",x="") + 
      annotate("text",
               x=-11,
               y=21,
               hjust=0,
               label=dataLabel,
               size=3,
               color="grey40") +
      theme_minimal(base_family = "serif") +
      theme(panel.background = element_rect(fill = "darkslategrey"))
    
    ggsave(file=fName,plot=p,dpi=600,width=7,height=6)
  }
}

# just one plot ####

locs <- filter(locsRaw, grepl("amphitheatre",featureTypes))
locPer <- filter(locs, grepl("roman",timePeriodsKeys))

ggplot(locPer) +
  coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap, 
               mapping = aes(X,Y,group=PID), 
               size = 0.1, 
               colour = landborder,
               fill = land,
               alpha = 1) +
  geom_point(
    data = locsRaw,
    aes(reprLong, reprLat),
    color = "grey70",
    alpha = .75,
    size = 1) + 
  geom_point(aes(y=reprLat,x=reprLong),
             color="salmon",
             alpha=.75,
             size=1) + 
  labs(title = "Amphitheatres in the Roman period", y="",x="") + 
  annotate("text",
           x=-11,
           y=21,
           hjust=0,
           label=dataLabel,
           size=3,
           color="grey40") +
  theme_minimal(base_family = "serif") +
  theme(panel.background = element_rect(fill = "darkslategrey"))

sf_locPer <- locPer %>% filter(!is.na(reprLong)) %>% filter(!is.na(reprLat))
sp::coordinates(sf_locPer) <- c("reprLong", "reprLat")
sp::proj4string(sf_locPer) <- sp::CRS("+init=epsg:4326")

# on how to use Github with R see https://r-pkgs.org/git.html
remotes::install_github("r-spatial/mapview")
mapviewOptions(
  basemaps = c("Esri.WorldShadedRelief", "Stamen.TonerLite"),
  raster.palette = grey.colors,
  na.color = "magenta",
  layers.control.pos = "topleft",
  fgb = F
)
mvMap <- mapview(sf_locPer, zcol = "featureTypes", burst = TRUE) 
mapshot(mvMap, url = paste0(getwd(), "/myMap.html"))


xlim <- c(5,16)
ylim <- c(47,55)

locs <- filter(locsRaw, grepl("fort",featureTypes))
locPer <- filter(locs, grepl("late-antique",timePeriodsKeys))

ggplot(locPer) +
  coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap,
               mapping = aes(X,Y,group=PID),
               size = 0.1,
               colour = landborder,
               fill = land,
               alpha = 1) +
  geom_point(
    data = locsRaw,
    aes(reprLong, reprLat),
    color = "grey70",
    alpha = .75,
    size = 1) +
  geom_point(aes(y=reprLat,x=reprLong),
             color="salmon",
             alpha=.75,
             size=1) +
  labs(title = "classical-time forts in Germany", y="",x="") +
  theme_minimal(base_family = "serif") +
  
  theme(panel.background = element_rect(fill = "darkslategrey"))


