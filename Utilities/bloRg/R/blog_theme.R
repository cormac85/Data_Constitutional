#' Blog Theme
#'
#' A ggplot2 object containing the theme settings for plots in my personal blog
#'
#' @author Cormac Nolan
#' @keywords data
#' @format ggplot2 object.
#' \describe{\item{blog_theme{ggplot2 object containing values for the theme.}}}
#'
"blog_theme"


#
# library(ggplot2)
#
# theme <-
#   ggplot2::theme(plot.title = element_text(face = 2, size = 18, hjust = 0),
#                  axis.title = element_text(colour = bloRg::blog_palette[2],
#                                            face = 2, size = 14),
#                  axis.text = element_text(colour = bloRg::blog_palette[2],
#                                           size = 12),
#                  axis.text.x = element_text(hjust = 1, vjust = 1,
#                                             angle = 45),
#                  strip.background =
#                    element_rect(fill = bloRg::blog_palette[2]),
#                  strip.text = element_text(face = "bold", colour = "white",
#                                            size = rel(1.2), vjust = 0.5),
#                  panel.background =
#                    element_rect(fill = paste0(bloRg::blog_palette[6], 30)),
#                  panel.grid.minor = element_blank(),
#                  panel.grid.major.x = element_blank(),
#                  panel.grid.major.y = element_line(colour = "white",
#                                                    size = 1))
#
# save(theme, file = "blog_theme.rda")
