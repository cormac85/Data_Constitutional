# Author: Cormac Nolan
# Date: 07/01/2015

# This file if for analysing the GHG emissions
# produced by Ireland with figures from the CSO.ie.
# The GHG emissions will be analysed in relation to 
# other metrics, economic or otherwise.

library(blorg)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(datakindr) # github/cormac85/datakindr

if(!interactive()){
  setwd("../")
}

# Import
raw.ghg.data <- datakindr::get_cso_dataset("EAA09")
irish.population <- datakindr::get_cso_dataset("PEA11")


irish.population <- 
  irish.population %>% 
  filter(Sex == "Both sexes", `Single Year of Age` == "All ages") %>% 
  select(`Year`, value) %>% 
  rename(population = value)

# Clean
# Multiply by conversion factors
# From here: http://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator
clean.ghg.data <- 
  raw.ghg.data %>% 
  mutate(statistic.short =
           case_when(Statistic == raw.ghg.data$Statistic[1] ~ "TOTAL",
                     Statistic == raw.ghg.data$Statistic[2] ~ "CO2",
                     Statistic == raw.ghg.data$Statistic[3] ~ "N2O",
                     Statistic == raw.ghg.data$Statistic[4] ~ "CH4")) %>% 
  mutate(co2.equivalent =
           case_when(statistic.short == "N2O" ~ value * 298,
                     statistic.short == "CH4" ~ value * 25,
                     TRUE ~ value))
  
# Totals Plot
ggplot(clean.ghg.data %>% filter(statistic.short == "TOTAL",
                                 `Sector NACE Rev 2` == "Total emissions"), 
       aes(Year, value,
           colour = statistic.short, group=1)) +
  geom_line(size = 1.5) +
  blog_theme +
  scale_color_manual(values = blog_palette)


p.ghg <-
  ggplot(clean.ghg.data %>% filter(statistic.short != "TOTAL",
                                 `Sector NACE Rev 2` == "Total emissions"),
       aes(Year, co2.equivalent, colour = statistic.short, 
           group = statistic.short)) +
  geom_line(size = 1.5) +
  blog_theme +
  scale_color_manual(values = blog_palette, name = "GHG Type") +
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016)) +
  labs(title = "Irish Contribution GHG's in CO2 Equivalent",
       y = "Total Emissions In CO2 Equivalent (1000 Tons)") +
  theme(legend.position = "top")

p.ghg

enriched.ghg <- 
  clean.ghg.data %>% 
  left_join(irish.population, by = "Year") %>% 
  mutate(Year = as.numeric(Year))
  
# Plot Combined Plot

p.population <-
  ggplot(enriched.ghg %>% filter(`Sector NACE Rev 2` == "Total emissions"),
         aes(Year, population)) +
  geom_line(size = 1.5, group = 1) +
  blog_theme +
  scale_color_manual(values = blog_palette) +
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(3.8e6, 4.2e6, 4.6e6),
                     labels = c("3.8", "4.2", "4.6")) +
  theme(axis.text.y = element_text(margin = margin(0, 0.7, 0, 0, "cm"))) +
  labs(y = "Population (millions)")

grid.arrange(p.ghg, p.population,
             heights = c(.75, .25), ncol = 1, nrow = 2)

# Read in the GNP figures (N1605 - statbank code)
irish.gni <- 
  read.csv("./data/ireland_gdp_gnp.csv")  
 
enriched.ghg <-
  enriched.ghg %>% 
  left_join(irish.gni, by = c("Year" = "year")) %>% 
  mutate(gnp.per.capita = gnp / population, 
         gdp.per.capita = gdp / population,
         ghg.emissions.per.capita = co2.equivalent / population)

# Get GNI per Capita and GHG per Capita plots
p.gni <-
  ggplot(enriched.ghg, aes(Year, gnp.per.capita)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2015)) +
  labs(title = "Irish Gross National Product per Capita (2000 - 2015)",
       y = "GNI Per Capita (€'000,000)")

p.ghg.per.capita <-
  ggplot(filter(enriched.ghg, statistic.short == "TOTAL", 
                `Sector NACE Rev 2` == "Total emissions"), 
                aes(Year, ghg.emissions.per.capita)) +
  geom_line(size = 1.5, colour = blog_palette[6]) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2015)) +
  theme(axis.text.y = element_text(margin = margin(0, 0.15, 0, 0, "cm"))) + # for lining up
  labs(title = "Irish Total GHG Emissions per Capita (2000 - 2015)",
       y = "GHG Emissions Per Capita ('000 Tonnes)")

# Plot in vertical arrangement
grid.arrange(p.gni, p.ghg.per.capita,
             heights = c(.5, .5), ncol = 1, nrow = 2)

# Ratio of GHG's to GNI
enriched.ghg <-
  enriched.ghg %>% 
  mutate(ghg.gnp.ratio = (co2.equivalent * 1000) / gnp,
         gnp.ghg.ratio = gnp / (co2.equivalent * 1000))

ggplot((enriched.ghg %>% filter(`Sector NACE Rev 2` == "Total emissions")), 
       aes(Year, ghg.gnp.ratio, 
           group = statistic.short, colour = statistic.short)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  scale_color_manual(values = blog_palette, name = "GHG Type") +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015)) +
  labs(title = "Irish Ratio of GHG Equivalent to GNP 2000 - 2015",
       y = "Ratio of GHG Equivalent to GNP (tonnes per million Euro)") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)
