library(tidyverse)

readings <-
  tibble::tribble(
    ~date,	~meter_reading, ~reading_type, ~energy_usage, ~unit,
    '12 Jun 2019',	94025, '(A)', 110, 'kWh',
    '5 Jun 2019',	93915, '(C)', 972, 'kWh',
    '8 Apr 2019',	92943, '(E)', 685, 'kWh',
    '6 Feb 2019',	92258, '(A)', 1034, 'kWh',
    '8 Dec 2018',	91224, '(E)', 481, 'kWh',
    '2 Oct 2018',	90743, '(A)', 292, 'kWh') %>% 
  dplyr::mutate(date = as.Date.character(date, format = "%d %b %Y"))

readings %>% 
  ggplot2::ggplot(ggplot2::aes(date, meter_reading, colour = reading_type)) +
  ggplot2::geom_point()

readings %>% 
  ggplot2::ggplot(ggplot2::aes(date, energy_usage, colour = reading_type)) +
  ggplot2::geom_point()

readings_model <- lm(formula = meter_reading ~ date, data = readings)

summary(readings_model)


readings$fitted_values <- 
  readings_model$fitted.values

readings %>% 
  ggplot2::ggplot(ggplot2::aes(date, meter_reading, colour = reading_type)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(ggplot2::aes(date, fitted_values), inherit.aes = FALSE) +
  ggplot2::geom_point(ggplot2::aes(date, fitted_values), inherit.aes = FALSE)
