install.packages("formattable")
install.packages("reshape")
library(tidyverse)
library(baseballr)
library(pitchRx)
library(dplyr)
library(formattable)
library(reshape)

#loading csv
ballpark <- read.csv("ballparkHR.csv")

#loading Seattle ballpark only
SEA <- ballpark %>%
  filter(home_team == 'SEA')

#getting rid of NA columns in Seattle ballpark variable
SEA <- SEA[colSums(!is.na(SEA)) > 0]

SEA %>%  distinct(pitch_name)

#chose to use -1.5 and 1.5 feet away from 0 (center of plate) because we're 
#focusing on pitches that are hits, or near/within strike zone
fastballs_mod <- fastballs %>%
  filter((plate_x > -1.5) & (plate_x < 1.5))

#chose to use 0.80 and 4.25 feet above the ground because we're 
#focusing on pitches that are hits, or near/within strike zone
fastballs_modx <- fastballs_mod %>%
  filter((plate_z > 0.80) & (plate_z < 4.25))

#used to create strike zone
x <- c(-.95, .95, .95, -.95, -.95)
z <- c(1.6, 1.6, 3.5, 3.5, 1.6)
sz <- tibble(x, z)

#visualizing pitch spray chart of all HR @ SEA with pitch_name
ggplot()+
  geom_path(data = sz, aes(x = x, y = z))+
  coord_equal()+
  xlab("Width of Home Plate")+
  ylab("Feet Above the Ground")+
  geom_point(data = SEA, aes(x = plate_x,y = plate_z, color=pitch_name))+
  scale_size(range = c(0.01, 3))

#used for heatmap
location <- fastballs_modx %>% select(plate_x, plate_z)

location <- as.data.frame(location)

location_melt <- melt(location)



#create heatmap of all FF pitches
