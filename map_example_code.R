# Install packages
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "lwgeom", "sf", "rnaturalearth", 
                   "rnaturalearthdata", "usmap"))
# load libraries, set theme
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library("usmap")


#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)
#
#ggplot(data = world) + geom_sf()

plot_usmap(regions = "states", exclude = c("AK", "HI")) +
  labs(title = "Continental U.S. States",
       subtitle = "Blank Map") +
  theme(panel.background =element_blank())
