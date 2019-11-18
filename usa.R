library(USAboundaries)
library(rmapshaper)
library(sf)
library(tidyverse)

#get the continental USA
usa <- us_states() 
usa <- subset(usa, !(state_abbr %in% c('PR', 'AK', 'HI')))  %>%  st_transform(2163)

#extract the inner bounadries
borders <- usa %>% 
  ms_innerlines() %>% 
  as_tibble() %>% 
  st_as_sf()

#get the length of each border. Lengths are likely to be a few miles off because of source data resolution + projections
borders$len <- st_length(borders) %>% as.numeric()
borders$mi <- round(borders$len * 0.000621371, 0) %>% as.numeric()

#extract the coordinates of each border
coords <- borders %>% st_coordinates() %>% as.data.frame()

#clean formatting for plotting
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.border=element_blank(), panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA))

#loop through all the borders
for (i in 1:nrow(borders)) {
  #figure out which states are on either side of the border
  intersect <- st_intersection(borders[i, ], usa)
  intersect$len <- st_length(intersect) %>% as.numeric() %>% round(0)
  intersect <- subset(intersect, len > 0)
  states <- subset(usa, state_abbr %in% intersect$state_abbr)
  
  #plot it
  ggplot() +  xlim(min(coords$X), max(coords$X)) + ylim(min(coords$Y), max(coords$Y)) + blankbg +
    geom_sf(data = borders[i, ], aes(colour = pctlen),fill = NA, size = .75) +
    scale_color_gradientn(colors = colors, limits = c(0, 1), guide = FALSE) +
    labs(title = paste0(intersect$name, collapse = "-"),
         subtitle = paste0(borders$mi[i], "mi"))
  
  #name the file the names of the states
  name <- paste0(round(borders$mi[i], 0), "_", paste0(intersect$state_abbr, collapse = "_"))
  
  #and save
  ggsave(paste0("./States/", name, ".png"), plot = last_plot(),
         scale = 1, width = 10, height = 6, units = "in",
         dpi = 500,   bg = "transparent")
  
}


