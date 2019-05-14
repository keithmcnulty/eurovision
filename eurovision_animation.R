library(tidyverse)
library(gganimate)
library(gifski)

# get data
eurovision_total <- readRDS("eurovision_total.RDS")

# set theme
my_font <- 'Oswald'
my_background <- "#FAEBD7"
my_pal <- c('#FB9F9F') #colors for bars (from wesanderson)
my_theme <- theme(text = element_text(),
                              rect = element_rect(fill = my_background),
                              plot.background = element_rect(fill = my_background, color = NA),
                              panel.background = element_rect(fill = my_background, color = NA),
                              panel.border = element_blank(),
                              plot.title = element_text(color = "#003366", face = 'bold', size = 30),
                              plot.subtitle = element_text(size = 20),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_line(color = 'grey75'),
                              panel.grid.minor.x = element_line(color = 'grey75'),
                              legend.position = 'none',
                              plot.caption = element_text(color = "#003366", size = 16),
                              axis.ticks = element_blank(),
                              axis.text.y =  element_blank(),
                  plot.margin = margin(2, 2, 2, 5, 'cm'))

theme_set(theme_light() + my_theme)

#  use Eurovision logo
img <- png::readPNG("eurovision-logo.png")
rast <- grid::rasterGrob(img, interpolate = T)

# create animated chart
eurovision_anim <- ggplot(aes(Order, group = Country), data = eurovision_total) +
  geom_tile(aes(y = Points / 2, 
                height = Points,
                width = 0.9, fill = my_pal), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  ggplot2::geom_text(aes(y = 0, label = Country), family = my_font, nudge_y = -600, size = 12) +
  geom_text(aes(y = Points, label = as.character(Points)), family=my_font, nudge_y = 250, size = 12) +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  labs(title = 'All-time Eurovision Total Points',
       subtitle = 'Finals Only',
       caption = 'Data source: Wikipedia | Plot by @dr_keithmcnulty',
       x = '',
       y = '') +
  # expand chart limits for design purposes
  expand_limits(y = c(-1500:7000), x = 11) +
  # add Eurovision logo in top right
  annotation_custom(rast, ymin = 6000, ymax = 7000, xmin = 10, xmax = 11) +
  # add Year underneath Eurovision logo
  geom_text(aes(x = 9.9, y = 6500, label=paste0(Year)), family=my_font, size = 14, color = '#003366') +
  # add annual winners in bottom right
  geom_text(aes(x = 1, y = 6300, label=paste0("Winner: ", Winner)), family=my_font, size = 10, color = '#003366') +
  transition_states(Year, 
                    transition_length = 1, state_length = 1) +
  ease_aes('cubic-in-out')

# save as preferred rendered format

gganimate::animate(eurovision_anim, 200, fps = 5, duration = 200, width = 2000, height = 1200, renderer = ffmpeg_renderer("anim.mp4"))
