# Author: Cormac Nolan
# Date: 07/01/2015

## The theme
library(ggplot2)
the.theme <- 
  ggplot2::theme(plot.title = element_text(face = 2, size = 18, hjust = 0),
                 axis.title = element_text(colour = bloRg::blog_palette[2],
                                           face = 2, size = 14),
                 axis.text = element_text(colour = bloRg::blog_palette[2],
                                          size = 12),
                 axis.text.x = element_text(hjust = 1, vjust = 1, 
                                            angle = 45),
                 strip.background = 
                   element_rect(fill = bloRg::blog_palette[2]),
                 strip.text = element_text(face = "bold", colour = "white",
                                           size = rel(1.2), vjust = 0.5),
                 panel.background =
                   element_rect(fill = paste0(bloRg::blog_palette[6], 30)),
                 panel.grid.minor = element_blank(),
                 panel.grid.major.x = element_blank(),
                 panel.grid.major.y = element_line(colour = "white",
                                                   size = 1))


# The palette
palette <- c('#AF240B', '#0B556D', '#AF540B', '#088031', '#E33048', 
             '#31778E', '#E39848', '#35065B', '#E3304850', '#31778E50',
             '#E3984850', '#35A65B50')
