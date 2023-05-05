install.packages("formattable")
install.packages("reshape")
install.packages('ggpubr') 

library(tidyverse)
library(baseballr)
library(dplyr)
library(ggpubr)
library(formattable)
library(reshape)

#loading csv
ballpark <- read.csv("ballparkHR.csv")

#loading Seattle ballpark only
SEA <- ballpark %>%
  filter(home_team == 'SEA')

#getting rid of NA columns in Seattle ballpark variable
SEA <- SEA[colSums(!is.na(SEA)) > 0]


#writing SEA data to separate csv
write.csv(SEA, "SEAballparkHR.csv", row.names = F)

SEA %>%  distinct(pitch_name)

#used to create strike zone
x <- c(-.8, 1.2, 1.2, -.8, -.8)
z <- c(1.5, 1.5, 3.5, 3.5, 1.5)
sz <- tibble(x, z)

#visualizing pitch spray chart of all HR @ SEA with pitch_name
SEAstrikezone <- ggplot()+
  geom_path(data = sz, aes(x = x, y = z))+
  coord_equal()+
  labs(title = "HR at T-Mobile by Pitch Type")+
  xlab("Width of Home Plate")+
  ylab("Feet Above the Ground")+
  geom_point(data = SEA, aes(x = plate_x,y = plate_z, color=pitch_name))+
  scale_size(range = c(0.01, 3))

#creating custom color scale
custom_color_scale <- scale_fill_gradientn(
  colors = c("#FBFBF9", "#0A1172","#3944BC", "#FF2800"),
  values = c(0, 0.3, .5, 1),
  guide = "colorbar"
)

#plotting heat map for HR in seattle
heatmapSEA <- ggplot(SEA, aes(x = plate_x, y = plate_z))+
  stat_density2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE)+
  custom_color_scale+
  geom_polygon(data = sz, aes(x, z), color = 'black', fill = NA)+
  theme_minimal()+
 xlab("Position on Home Plate")+
  ylab("Feet Above Ground")+
  labs(title = "Heatmap of HR at T-Mobile")

ggarrange(SEAstrikezone, heatmapSEA)
