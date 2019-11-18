library(rworldmap)
library(rmapshaper)
library(sf)
library(tidyverse)


#get a world map
world <- getMap(resolution = "high") %>%  st_as_sf() 

#then get borders
borders <- world %>%
  ms_innerlines() %>%
  st_as_sf()

borders <- borders %>% st_transform(54030)
world <- world %>% st_transform(54030)

#this isn't accurate but it's good enough for getting some nice colors in
borders$len <- st_length(borders) %>% as.numeric

#and plot it all
colors <- c('#12c2e9', '#c471ed', '#f64f59')
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.border=element_blank(), panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                text=element_text(family="Old Standard TT", size=8, color = '#909090'),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA))

ggplot() + blankbg +
  geom_sf(data = world, fill = '#edece8', color = NA) +
  geom_sf(data = borders, aes(color = len)) +
  scale_color_gradientn(colors = colors, guide = FALSE )

ggsave('world_withborder.png', plot = last_plot(),
       scale = 1, width = 14, height = 8, units = "in",
       dpi = 500,   bg = "transparent")



