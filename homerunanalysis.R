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
  xlab(NULL)+
  ylab("Feet Above the Ground")+
  geom_point(data = SEA, aes(x = plate_x,y = plate_z, color=pitch_name),
             na.rm = TRUE)+
  theme_pubr(base_size = 12, base_family = 'serif', legend = "right")


#plotting heat map for HR in seattle
heatmapSEA <- ggplot(SEA, aes(plate_x, plate_z))+
  stat_density_2d_filled(geom = "density_2d_filled", contour = TRUE, 
                         contour_var = "density", 
                         na.rm = TRUE, show.legend = TRUE)+
  geom_path(data = sz, aes(x, z), color = 'white')+
  xlab(NULL)+
  ylab("Feet Above Ground")+
  labs(title = "Home Run Density in Strikezone at T-Mobile", 
       fill = "Density Value")+
  theme_pubr(base_size = 12, base_family = 'serif', legend = "right")
  
ggarrange(SEAstrikezone, heatmapSEA)




