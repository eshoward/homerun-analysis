install.packages("formattable")
install.packages("reshape")
install.packages('ggpubr') 

library(tidyverse)
library(baseballr)
library(dplyr)
library(ggpubr)

#loading csv
ballpark <- read.csv("ballparkHR.csv")

#loading Seattle ballpark only
#SEA <- ballpark %>%
  #filter(home_team == 'SEA')

#getting rid of NA columns in Seattle ballpark variable
ballpark <- ballpark[colSums(!is.na(ballpark)) > 0]


#writing SEA data to separate csv
#write.csv(SEA, "SEAballparkHR.csv", row.names = F)

#SEA %>%  distinct(pitch_name)

#used to create strike zone
x <- c(-.8, 1.2, 1.2, -.8, -.8)
z <- c(1.5, 1.5, 3.5, 3.5, 1.5)
sz <- tibble(x, z)

#visualizing pitch spray chart of all HR with pitch_name
ballpark_strikezone <- ggplot()+
  geom_path(data = sz, aes(x = x, y = z))+
  coord_equal()+
  labs(title = "Home Runs by Pitch Type")+
  xlab(NULL)+
  ylab("Feet Above the Ground")+
  geom_point(data = ballpark, aes(x = plate_x,y = plate_z, color=pitch_name),
             na.rm = TRUE)+
  theme_pubr(base_size = 12, base_family = 'serif', legend = "right")


#plotting heat map for HR
heatmap_ballpark <- ggplot(ballpark, aes(plate_x, plate_z))+
  stat_density_2d_filled(geom = "density_2d_filled", contour = TRUE, 
                         contour_var = "density", 
                         na.rm = TRUE, show.legend = TRUE)+
  geom_path(data = sz, aes(x, z), color = 'white')+
  xlab(NULL)+
  ylab("Feet Above Ground")+
  labs(title = "Home Run Density in Strikezone", 
       fill = "Density Value")+
  theme_pubr(base_size = 12, base_family = 'serif', legend = "right")
  

plot_data <- ggplot_build(heatmap_ballpark)$data[[1]]

#finding HR that have are in the density <= 0.1
filtered_events <- plot_data %>% filter(piece == 1) %>%
  mutate(
    x= round(x,2),
    y= round(y,2))

#finding the events whose density <= 0.1 based off pitch location
table_data <- ballpark %>%
  filter(plate_x %in% filtered_events$x & plate_z %in% filtered_events$y)

#viewing events whose density is <= .1
ggplot()+
  geom_path(data = sz, aes(x = x, y = z))+
  coord_equal()+
  labs(title = "Home Runs by Pitch Type Where Density is <= 0.1")+
  xlab(NULL)+
  ylab("Feet Above the Ground")+
  geom_point(data = table_data, aes(x = plate_x,y = plate_z, color=pitch_name),
             na.rm = TRUE)+
  theme_pubr(base_size = 12, base_family = 'serif', legend = "right")

#barchart of pitch types that were hit for HR whose density was <= 0.1
ggplot(table_data, aes(x = reorder(pitch_name, -table(pitch_name)[pitch_name]),
                       fill = pitch_name))+
  geom_bar()+
  xlab("Pitch Name")+
  ylab("Count") +
  ggtitle("Count of Pitches Hit For HR Whose Density <= 0.1")+
  theme(text = element_text(family = 'serif', size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_blank())+
  guides(fill = 'none')

#viewing barrel contact of HR whose density was <= 0.1
ggplot(table_data, aes(x = factor(launch_speed_angle)))+
  geom_bar(na.rm = TRUE)+
  scale_x_discrete(labels = c("Under", "Flare/Burner", "Solid Contact", "Barrel"),
                   na.translate = FALSE)+
  xlab("Barrel Contact")+
  ylab("Count") +
  ggtitle("Categorization of Barrel Contact by Count")+
  theme(text = element_text(family = 'serif', size = 14), 
        panel.background = element_blank())

#filtering out for barreled balls
barreled <- table_data %>%
  filter(launch_speed_angle == 6)

#finding median values for launch speed, angle, and hit distance
median(ballpark$launch_speed, na.rm=TRUE)
median(ballpark$launch_angle, na.rm = TRUE)
median(ballpark$hit_distance_sc, na.rm = TRUE)

#finding median distance of pitch types
mediandist_bypitch <- barreled %>%
  group_by(pitch_name) %>%
  summarise(median_distance = median(hit_distance_sc, na.rm = TRUE))

#dropping blank row, which is same as 'other'
mediandist_bypitch <- mediandist_bypitch %>% slice(-1)

ggplot(mediandist_bypitch, aes(x = pitch_name,y = median_distance))+
  geom_point()+
  geom_text(aes(label = median_distance), vjust = -0.8, size = 3)+
  xlab("Pitch Type")+
  ylab("Median Distance Hit")+
  ggtitle("Median Distance Hit by Pitch Type")+
  theme(text = element_text(family = 'serif', size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_blank())+
  guides(fill = 'none')








