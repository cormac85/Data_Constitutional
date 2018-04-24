#' Create Meaningless But Pretty Plots
#'
#' @param n Number of points in the plot
#' @param plot_type Choose plot type, 1 is far away, 2 is zoomed in
#' @param num_cat Number of categories to plot with the colours
#'
#' @return
#'
#' @examples
generate_pretty_plot <- function(n, plot_type, num_cat, point_size = 0.75){

  cat_probs <-
    case_when(
      num_cat == 0 ~ list(1),
      num_cat == 1 ~ list(1),
      num_cat == 2 ~ list(c(0.35, 0.65)),
      num_cat == 3 ~ list(c(0.25, 0.35, 0.4)),
      num_cat == 4 ~ list(c(0.2, 0.3, 0.3, 0.2)),
      num_cat == 5 ~ list(c(0.2, 0.3, 0.2, 0.1, 2)),
      TRUE ~ list("too many characters")
    )

  lm_sim <- tibble(x = rbeta(n, 10, 7),
                   y = rpois(n, 50) + rnorm(n, 0, 1),
                   categories = sample( LETTERS[1:num_cat], n,
                                        replace = TRUE,
                                        prob = cat_probs[[1]] ))

  lm_sim_final <-
    lm_sim %>%
    mutate(noisy_y = y - y / x) %>%
    modelr::add_predictions(model = lm(noisy_y ~ poly(x,6),
                                       data = lm_sim_final))
  if(plot_type == 1){
    lm_sim_final %>%
      ggplot(aes(x,noisy_y, colour = categories)) +
      geom_point(alpha = 1, size = point_size) +
      geom_line(aes(x, pred), colour = "black", linetype = 1, size = 1) +
      theme_void() +
      scale_colour_brewer(type = "qual", palette = 6) +
      theme(legend.position = "none")

  } else if (plot_type == 2){
    lm_sim_final %>%
      ggplot(aes(x,noisy_y, colour = categories)) +
      geom_point(alpha = 1, size = point_size) +
      geom_smooth(se = FALSE, colour = "black", formula = y ~ poly(x,2),
                  method = "lm") +
      xlim(0.4, 0.7) + ylim(-70, -20) +
      theme_void() +
      scale_colour_brewer(type = "qual", palette = 6) +
      theme(legend.position = "none")
  }
}
