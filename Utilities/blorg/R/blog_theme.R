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
#
# library(ggplot2)
#
# blog_theme <-
#   ggplot2::theme(plot.title = element_text(face = 2, size = 18, hjust = 0,
#                                            colour = blorg::blog_palette[2]),
#                  axis.title = element_text(colour = blorg::blog_palette[2],
#                                            face = 2, size = 14),
#                  axis.text = element_text(colour = blorg::blog_palette[2],
#                                           size = 12),
#                  axis.text.x = element_text(hjust = 1, vjust = 1,
#                                             angle = 45),
#                  axis.ticks.y = element_blank(),
#                  axis.ticks.x = element_line(colour = blorg::blog_palette[2]),
#                  strip.background =
#                    element_rect(fill = blorg::blog_palette[2]),
#                  strip.text = element_text(face = "bold", colour = "white",
#                                            size = rel(1.2), vjust = 0.5),
#                  panel.background =
#                    element_rect(fill = paste0(blorg::blog_palette[6], 30)),
#                  panel.grid.minor = element_blank(),
#                  panel.grid.major.x = element_blank(),
#                  panel.grid.major.y = element_line(colour = "white",
#                                                    size = 1))
#
# save(blog_theme, file = "./data/blog_theme.rda")
#
# Test:
# ggplot2::ggplot(data.frame(a = c(1,2,3), b = c("a", "b", "c")),
# ggplot2::aes(x = b, y = a, fill = b)) +
#   ggplot2::geom_bar(stat = "identity") +
#   ggplot2::labs(title = "blah") +
#   blorg::blog_theme +
#   ggplot2::scale_fill_manual(values = blorg::blog_palette)

