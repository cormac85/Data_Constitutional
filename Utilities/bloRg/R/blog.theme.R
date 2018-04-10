# Author: Cormac Nolan
# Date: 12/09/2015
# Purpose: Creates a theme for use with ggplot2 inspired by The Economist
# magazine style of plots.

# pal1 = c("#AA4A39", "#26596A", "#709C34", # normal
#          "#802615", "#104050", "#4C7513",
#          "#550D00", "#022835", "#2D4e00", # dark
#          "#FFB7AA","#6C939F", "#C9EA9C") #light
#
# pal2 = c("#AF240B", "#0B556D", "#61A00A", # Normal
#          "#FF2800", "#09C2FF","#94FF00",  # Light
#          "#E36048", "#31778E", "#94D042",# lighter
#          "#911600", "#03455A", "#4D8400") # dark

# http://paletton.com/palette.php?uid=7070u0kt%2BlZlOstrKqzzSiaJidt
pal3 = c("#AF240B", "#0B556D","#AF540B", "#088031", # normal
         "#E33048", "#31778E", "#E39848", "#35065B", # Light
         "#E3304850", "#31778E50", "#E3984850", "#35A65B50") # Super Light

blog.palette = pal3

blog.theme <-
  theme(plot.title = element_text(size = 18, face=2, hjust=0,
                                  color=blog.palette[2]),
        axis.title= element_text(size = 14, face=2, color=blog.palette[2]),
        axis.text = element_text(size = 12, colour = blog.palette[2]),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        strip.background = element_rect(fill = blog.palette[2]),
        strip.text = element_text(colour = "white", face = "bold",
                                  size = rel(1.2), vjust= 0.5),
        panel.background = element_rect(fill = "#31778E30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = paste0(blog.palette[6], "75"),
                                          size = 0.1))

rm(pal3)
