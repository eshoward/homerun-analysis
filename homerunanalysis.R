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

#creating strike zone. Below is trying to create heatmap.
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95

kZone <- tibble(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y = c(botKzone, topKzone, topKzone, botKzone, botKzone))

x <- seq(-1.5, 1.5, length.out = 50)
y <- seq(.5, 5, length.out = 50)
data.predict <- data.frame(X = c(outer(x,y * 0 + 1)),
                           Z = c(outer(x * 0 + 1, y)))
lp <- predict(fit, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))

ggplot(kZone, aes(x = x, y = y))+
  geom_tile(data = SEA, aes(x = plate_x, y = plate_z, fill = release_speed))+
  scale_fill_distiller(palette = "Spectral")+
  geom_path(lwd = .5, col = "black")+
  coord_fixed()

