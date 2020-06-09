####---
# Title: RodentConsumptionMap.R
#
# Author: Mark Myer
#
# Date: 6/09/20
#
# Purpose: To map rodent bait consumption in New Orleans
#
# R version 3.6.1 Action of the Toes
####---

library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sp)

# #Import and convert kml maps to Excel
# fq <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/FQ_040320.kml", sep = "/"), stringsAsFactors = FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(fq, file= "BourbonFQ.csv")
# 
# parks <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/Targeted 4220.kml", sep = "/"), stringsAsFactors = FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(parks, file= "Hotspots.csv")
# 
# ware <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/Warehouse 4220.kml", sep = "/"), stringsAsFactors = FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(ware, file= "CBDWarehouse.csv")
# 
# cityhall <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/Rodent Bait Stations City Hall 4_6_20-0.kml", sep = "/"), stringsAsFactors= FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(cityhall, file= "CityHall.csv")
# 
# oak <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/Rodent Bait Oak St. 4_8_20-0.kml", sep = "/"), stringsAsFactors= FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(oak, file= "Oak.csv")
# 
# julia <- readOGR( dsn = paste( getwd(), "/Rodent Baits/Mapping Files/Rat Stations Julia And Baronne 4.13.2020.kml", sep = "/"), stringsAsFactors= FALSE) %>%
#   data.frame() %>% 
#   select(Name, Long = coords.x1, Lat = coords.x2) 
# write.csv(julia, file= "Julia.csv")

#Read them back in with appended consumption data
fq <- read.csv("./Rodent Baits/CSVs/BourbonFQ.csv")
parks <- read.csv("./Rodent Baits/CSVs/Hotspots.csv")
ware <- read.csv("./Rodent Baits/CSVs/CBDWarehouse.csv")
oak <- read.csv("./Rodent Baits/CSVs/Oak.csv")

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

# #Percent consumption
# tiff(filename = "./Rodent Baits/Maps/FQ_Cons_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
# map + geom_point(data = filter(fq, Con_51120 >0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_60220))  + 
#   geom_point(data = fq, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
#   scale_size_continuous(limits = c(5, max(fq$Con_51120, na.rm=T))) +
#   guides() +
#   labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
#   ggtitle("FQ Bait Consumption 6/09/2020") +
#   theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
#         axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
#         plot.title = element_text(size=20, face = "bold"))
# dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Rodent Baits/Maps/FQ_Cumulative_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(fq[order(fq$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  #geom_point(data = fq, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("French Quarter Rodent Surveillance 6/09/20") +
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

# tiff(filename = "./Rodent Baits/Maps/Ware_Cons_050620.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
# map + geom_point(data = filter(ware, Con_50620 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_50620))  + 
#   geom_point(data = ware, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
#   scale_size_continuous(limits = c(5, max(ware$Con_50620, na.rm=T))) +
#   guides() +
#   labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
#   ggtitle("Warehouse/CBD Bait Consumption 5/06/2020") +
#   theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
#         axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
#         plot.title = element_text(size=20, face = "bold"))
# dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Rodent Baits/Maps/Ware_Cumulative_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(ware[order(ware$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4,5))))  + 
  #geom_point(data = ware, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "orangered", "red"), drop = F) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("Warehouse/CBD Rodent Surveillance 6/09/20") +
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

# tiff(filename = "./Rodent Baits/Maps/Oak_Cons_051320.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
# map + geom_point(data = filter(oak, Con_51320 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_51320))  + 
#   geom_point(data = oak, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
#   scale_size_continuous(limits = c(5, max(oak$Con_41520, na.rm=T))) +
#   guides() +
#   labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
#   ggtitle("Oak St Bait Consumption 5/13/2020") +
#   theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
#         axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
#         plot.title = element_text(size=20, face = "bold"))
# dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Rodent Baits/Maps/Oak_Cumulative_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(oak[order(oak$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  #geom_point(data = oak, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red"), drop = F) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("Oak St Rodent Surveillance 6/09/20") +
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

# tiff(filename = "./Rodent Baits/Maps/CBDHotspots_Cons_050120.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
# map + geom_point(data = filter(parks, Con_50120 > 0), pch=19, col="red", alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, size = Con_50120))  + 
#   geom_point(data = parks, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
#   scale_size_continuous(limits = c(5, max(parks$Con_50120, na.rm=T))) +
#   guides() +
#   labs(x = "Longitude", y = "Latitude", size = "Consumption %") + 
#   ggtitle("CBD Hotspots Consumption 5/01/2020") +
#   theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
#         axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
#         plot.title = element_text(size=20, face = "bold"))
# dev.off()

#Cumulative consumption - number of hits
tiff(filename = "./Rodent Baits/Maps/CBD_Cumulative_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(parks[order(parks$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  #geom_point(data = parks, pch = 20, col ="black", size = 0.6, aes(x=Long, y=Lat)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red"), drop=F) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("CBD Hotspots Rodent Surveillance 6/09/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()


#Make line graphs of consumption over time separately for each site and then also on the same graph
#This is a gross way to do this but it works
fq_line = data.frame(week = c(mdy("04/07/2020", "04/14/2020", "04/20/2020", "04/28/2020", "05/04/2020", "05/11/2020", "05/19/2020", "05/26/2020", "06/02/2020")), 
                     hits = c(sum(fq$Con_4720>0), sum(fq$Con_41420>0), sum(fq$Con_42020>0), sum(fq$Con_42820>0), sum(fq$Con_50420>0), sum(fq$Con_51120>0), sum(fq$Con_51920>0), sum(fq$Con_52620>0), sum(fq$Con_60220>0)))

tiff(filename = "./Rodent Baits/Graphs/FQ_Line060920.tiff", height = 4, width = 4.5, units = "in", res = 120, compression = "lzw", type = "cairo")
ggplot(data = fq_line, aes(x = week, y = hits)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks = c(seq(0,max(fq_line$hits), by = 2))) + 
  labs(title = "French Quarter", x="Date", y="Rat Bait Hits") + 
  geom_text(label = paste0("N = ", nrow(fq), " Bait Stations"),
            x = as.Date(yday(max(fq_line$week)) - ((yday(max(fq_line$week)) - yday(min(fq_line$week))) /2), origin = "2020-01-01"),
            y = max(fq_line$hits) * 0.9) +
  theme_minimal()
dev.off()

oak_line = data.frame(week = c(mdy("04/15/2020", "04/30/2020", "05/05/2020", "05/13/2020", "05/20/2020", "05/26/2020")), 
                     hits = c(sum(oak$Con_41520>0, na.rm=T), sum(oak$Con_43020>0, na.rm=T), sum(oak$Con_50520>0, na.rm=T), sum(oak$Con_51320>0, na.rm=T),  sum(oak$Con_52020>0, na.rm=T), sum(oak$Con_52620>0, na.rm=T)))

tiff(filename = "./Rodent Baits/Graphs/Oak_Line060920.tiff", height = 4, width = 4.5, units = "in", res = 120, compression = "lzw", type = "cairo")
ggplot(data = oak_line, aes(x = week, y = hits)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks = c(seq(0,max(oak_line$hits), by = 2))) + 
  labs(title = "Oak St", x="Date", y="Rat Bait Hits") + 
  geom_text(label = paste0("N = ", nrow(oak), " Bait Stations"),
            x = as.Date(yday(max(oak_line$week)) - ((yday(max(oak_line$week)) - yday(min(oak_line$week))) /2), origin = "2020-01-01"),
            y = max(oak_line$hits) * 0.9) +
  theme_minimal()
dev.off()

ware_line = data.frame(week = c(mdy("04/09/2020", "04/16/2020", "05/01/2020", "05/06/2020", "05/21/2020", "05/28/2020", "06/04/2020")), 
                       hits = c(sum(ware$Con_4920>0, na.rm=T), sum(ware$Con_41620>0, na.rm=T), sum(ware$Con_50120>0, na.rm=T), sum(ware$Con_50620>0, na.rm=T), sum(ware$Con_52120>0, na.rm=T), sum(ware$Con_52820>0, na.rm=T), sum(ware$Con_60420>0, na.rm=T)))

tiff(filename = "./Rodent Baits/Graphs/Ware_Line060920.tiff", height = 4, width = 4.5, units = "in", res = 120, compression = "lzw", type = "cairo")
ggplot(data = ware_line, aes(x = week, y = hits)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks = c(seq(0,max(ware_line$hits), by = 2))) + 
  labs(title = "Warehouse District/CBD", x="Date", y="Rat Bait Hits") + 
  geom_text(label = paste0("N = ", nrow(ware), " Bait Stations"),
            x = as.Date(yday(max(ware_line$week)) - ((yday(max(ware_line$week)) - yday(min(ware_line$week))) * 0.25), origin = "2020-01-01"),
            y = max(ware_line$hits) * 0.9) +
  theme_minimal()
dev.off()

parks_line = data.frame(week = c(mdy("04/15/2020", "05/01/2020", "05/20/2020", "06/04/2020")), 
                        hits = c(sum(parks$Con_41520>0, na.rm=T), sum(parks$Con_50120>0, na.rm=T), sum(parks$Con_52020>0, na.rm=T), sum(parks$Con_60420>0, na.rm=T)))

tiff(filename = "./Rodent Baits/Graphs/Parks_Line060920.tiff", height = 4, width = 4.5, units = "in", res = 120, compression = "lzw", type = "cairo")
ggplot(data = parks_line, aes(x = week, y = hits)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks = c(seq(0,max(parks_line$hits), by = 2))) + 
  labs(title = "CBD Hotspots", x="Date", y="Rat Bait Hits") + 
  geom_text(label = paste0("N = ", nrow(parks), " Bait Stations"),
            x = as.Date(yday(max(parks_line$week)) - ((yday(max(parks_line$week)) - yday(min(parks_line$week))) /2), origin = "2020-01-01"),
            y = max(parks_line$hits) * 0.9) +
  theme_minimal()
dev.off()


#Overlay bait consumption on restaurants in FQ 
#Import restaurant shapefile
rest <- readOGR(dsn = "/Users/mark/OneDrive - City of New Orleans/GIS and Data/Restaurants", layer = "geo_export_a03cff17-dafa-4a9d-8f58-9148179aa597")
rest.coord <- data.frame(rest@coords)

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

#FQ Cumulative Consumption with Restaurant Overlay 
tiff(filename = "./Rodent Baits/Maps/FQ_RestaurantOverlay_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(fq[order(fq$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  geom_point(data = rest.coord, pch = 20, col ="black", size = 1, aes(x= coords.x1, y=coords.x2)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red")) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("French Quarter Rodent Surveillance 6/09/20") +
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

#Oak St Cumulative with Restaurant Overlay
tiff(filename = "./Rodent Baits/Maps/Oak_RestaurantOverlay_060920.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = filter(oak[order(oak$numHits),], numHits >0), pch=19, size = 6, alpha = 0.5, stroke = 1, aes(x=Long, y= Lat, col = factor(numHits, levels = c(1,2,3,4))))  + 
  geom_point(data = rest.coord, pch = 20, col ="black", size = 1, aes(x= coords.x1, y=coords.x2)) +
  scale_colour_manual(values = c("green", "yellow", "orange", "red"), drop = F) +
  guides() +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative Weeks\nBait Consumption") + 
  ggtitle("Oak St Rodent Surveillance 6/09/20") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"))
dev.off()
