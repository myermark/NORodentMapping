####---
# Title: RodentConsumptionMap.R
#
# Author: Mark Myer
#
# Date: 4/23/2020
#
# Purpose: To map rodent bait consumption in New Orleans
#
# R version 3.6.1 Action of the Toes
####---

library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)

#Import and convert kml maps to Excel
fq <- readOGR( dsn = paste( getwd(), "/GIS Files/FQ_040320.kml", sep = "/"), stringsAsFactors = FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(fq, file= "FQ_040320.csv")

parks <- readOGR( dsn = paste( getwd(), "/GIS Files/Targeted 4220.kml", sep = "/"), stringsAsFactors = FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(parks, file= "parks_040220.csv")

ware <- readOGR( dsn = paste( getwd(), "/GIS Files/Warehouse 4220.kml", sep = "/"), stringsAsFactors = FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(ware, file= "ware_040220.csv")

cityhall <- readOGR( dsn = paste( getwd(), "/GIS Files/Rodent Bait Stations City Hall 4_6_20-0.kml", sep = "/"), stringsAsFactors= FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(cityhall, file= "cityhall_040620.csv")

oak <- readOGR( dsn = paste( getwd(), "/GIS Files/Rodent Bait Oak St. 4_8_20-0.kml", sep = "/"), stringsAsFactors= FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(oak, file= "oak_040820.csv")

julia <- readOGR( dsn = paste( getwd(), "/GIS Files/Rat Stations Julia And Baronne 4.13.2020.kml", sep = "/"), stringsAsFactors= FALSE) %>%
  data.frame() %>% 
  select(Name, Long = coords.x1, Lat = coords.x2) 
write.csv(julia, file= "julia_040820.csv")

#Read them back in with appended consumption data
fq <- read.csv("./Bait Records/FQ_040320.csv")
parks <- read.csv("./Bait Records/parks_040220.csv")
ware <- read.csv("./Bait Records/ware_040220.csv")
oak <- read.csv("./Bait Records/oak_040820.csv")

#Get cumulative consumption data -- number of times bait hit per site
fq <- fq %>%
  mutate(numHits = rowSums((select(fq, 4:ncol(fq)) > 0)))
oak <- oak %>%
  mutate(numHits = rowSums((select(oak, 4:ncol(oak)) > 0)))
parks <- parks %>%
  mutate(numHits = rowSums((select(parks, 4:ncol(parks)) > 0)))
ware <- ware %>%
  mutate(numHits = rowSums((select(ware, 4:ncol(ware)) > 0)))

#Map the consumption data
#Visualize using ggmaps
#Get the French Quarter bounding box
height <- max(fq$Lat) - min(fq$Lat)
width <- max(fq$Long) - min(fq$Long)
borders <- c(bottom  = min(fq$Lat)  - 0.3 * height, 
             top     = max(fq$Lat)  + 0.3 * height,
             left    = min(fq$Long) - 0.3 * width,
             right   = max(fq$Long) + 0.3 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 16, maptype = "terrain")
map <- ggmap(nola_stamen)

#Percent consumption
tiff(filename = "./Maps/FQ_Cons_050420.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(fq, Con_50420 >0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_50420))  + 
  geom_point(data = fq, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_size_continuous(limits = c(5, max(fq$Con_50420, na.rm=T))) +
  guides() +
  labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
  ggtitle("FQ Bait Consumption 5/04/2020") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Maps/FQ_Cumulative_050420.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(fq[order(fq$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  geom_point(data = fq, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("French Quarter Rodent Surveillance 5/04/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()

#Get the Warehouse District/CBD bounding box
height <- max(ware$Lat) - min(ware$Lat)
width <- max(ware$Long) - min(ware$Long)
borders <- c(bottom  = min(ware$Lat)  - 0.3 * height, 
             top     = max(ware$Lat)  + 0.3 * height,
             left    = min(ware$Long) - 1 * width,
             right   = max(ware$Long) + 1 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 16, maptype = "terrain")
map <- ggmap(nola_stamen)

tiff(filename = "./Maps/Ware_Cons_050120.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(ware, Con_50120 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_50120))  + 
  geom_point(data = ware, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_size_continuous(limits = c(5, max(ware$Con_50120, na.rm=T))) +
  guides() +
  labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
  ggtitle("Warehouse/CBD Bait Consumption 5/01/2020") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Maps/Ware_Cumulative_050120.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(ware[order(ware$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2))))  + 
  geom_point(data = ware, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("Warehouse/CBD Rodent Surveillance 5/01/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()

#Get the Oak Street bounding box
height <- max(oak$Lat) - min(oak$Lat)
width <- max(oak$Long) - min(oak$Long)
borders <- c(bottom  = min(oak$Lat)  - 0.3 * height, 
             top     = max(oak$Lat)  + 0.3 * height,
             left    = min(oak$Long) - 0.3 * width,
             right   = max(oak$Long) + 0.3 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 16, maptype = "terrain")
map <- ggmap(nola_stamen)

tiff(filename = "./Maps/Oak_Cons_043020.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(oak, Con_43020 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_43020))  + 
  geom_point(data = oak, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_size_continuous(limits = c(5, max(oak$Con_41520, na.rm=T))) +
  guides() +
  labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
  ggtitle("Oak St Bait Consumption 4/30/2020") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Maps/Oak_Cumulative_043020.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(oak[order(oak$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2))))  + 
  geom_point(data = oak, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("Oak St Rodent Surveillance 4/30/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()

#Get the CBD Hotspots bounding box
height <- max(parks$Lat) - min(parks$Lat)
width <- max(parks$Long) - min(parks$Long)
borders <- c(bottom  = min(parks$Lat)  - 0.3 * height, 
             top     = max(parks$Lat)  + 0.3 * height,
             left    = min(parks$Long) - 0.3 * width,
             right   = max(parks$Long) + 0.3 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 16, maptype = "terrain")
map <- ggmap(nola_stamen)

tiff(filename = "./Maps/CBDHotspots_Cons_050120.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(parks, Con_50120 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_50120))  + 
  geom_point(data = parks, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_size_continuous(limits = c(5, max(parks$Con_50120, na.rm=T))) +
  guides() +
  labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
  ggtitle("CBD Hotspots Consumption 5/01/2020") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Maps/CBD_Cumulative_050120.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(parks[order(parks$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2))))  + 
  geom_point(data = parks, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("CBD Hotspots Rodent Surveillance 5/01/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()