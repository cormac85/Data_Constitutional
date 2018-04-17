# Author: Cormac Nolan
# Date: 07/01/2015

# This file if for analysing the GHG emissions
# produced by Ireland with figures from the CSO.ie.
# The GHG emissions will be analysed in relation to 
# other metrics, economic or otherwise.

library(blorg)
library(magrittr)
library(gridExtra)
library(grid)
library(scales)

calculate_per_capita_GHGs <- function(ghg.data,
                                      population){
  # Function to calculate the ghg per capita given populationa and ghg data
  
  ghg.data$population <- population$Population.Count
  
  ghg.data %>% 
    mutate(ghg.emmissions.tons.per.capita = 
             (Total.emissions * 1000) / population)
  
}

conversion.function <- function(x){
  
  # converts the tonnes of GHG's to tonnes of CO2 equivalents
  
  conversion.N2O <- 298
  conversion.CH4 <- 25
  
  
  x[27:39, 2:23] <- x[27:39, 2:23] * conversion.N2O
  
  x[40:52, 2:23] <- x[40:52, 2:23] * conversion.CH4
  
  x
}

# Import
raw.ghg.data <- datakindr::get_cso_dataset("EAA01")

# Clean
melted.ghg.data <-
  cso_category_clean(raw.ghg.data, category.num.rows = 14)

melted.ghg.data$X. <- NULL
melted.ghg.data$category.name <- NULL

melted.ghg.data <- 
  melted.ghg.data %>% 
  rename(Year = X..1,
         category = category.code)

# Create short categories
category.names <- unique(melted.ghg.data$category)
category.short.names <- c("Total GHG's", "CO2", "N2O", "CH4")

melted.ghg.data$category.short.names <- NA
for(i in 1:length(category.names)){
  
  melted.ghg.data[melted.ghg.data$category == category.names[i], 25] <-
    category.short.names[i]
    
}

# Totals Plot
ggplot(melted.ghg.data, aes(Year, Total.emissions, 
                            colour = category.short.names)) +
  geom_line(size = 1.5) +
  blog_theme +
  scale_color_manual(values = blog_palette)

# Multiply by conversion factors
# From here: http://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator




converted.ghg.data <-
  conversion.function(melted.ghg.data)


test.theme <- 
  theme(plot.title = element_text(colour = blog_palette[2],
                                  face = 2, size = 18, hjust = 0),
        axis.title = element_text(colour = blog_palette[2], face = 2,
                                  size = 14),
        axis.text = element_text(colour = blog_palette[2], size = 12),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45),
        strip.background = element_rect(fill = blog_palette[2]),
        strip.text = element_text(face = "bold", colour = "white",
                                  size = rel(1.2), vjust = 0.5),
        panel.background = element_rect(fill = paste0(blog_palette[6], 30)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "white",
                                        size = 1))

p.ghg.converted<-
  ggplot(converted.ghg.data, aes(Year, Total.emissions, 
                                 colour = category.short.names)) +
  geom_line(size = 1.5) +
  test.theme +
  scale_color_manual(values = blog_palette, name = "GHG Type") +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
  labs(title = "Irish Contribution GHG's in CO2 Equivalent",
       y = "Total Emissions In CO2 Equivalent (1000 Tons)") +
  theme(legend.position = "top")

p.ghg.converted

# Read in the population estimates.
irish.population <- read.csv("irish_population_estimates.csv", header = F,
                             col.names = c("Gender", "Year", 
                                           "Population.Count"))
irish.population %<>% 
  filter(Gender == "Both sexes") %>% 
  select(Year, Population.Count) %>% 
  filter((Year >= 2000) & (Year <= 2012))


converted.ghg.data <- 
  calculate_per_capita_GHGs(converted.ghg.data, irish.population)
  
# Plot Combined Plot

p.population <-
  ggplot(converted.ghg.data, aes(Year, population)) +
  geom_line(size = 1.5) +
  test.theme +
  scale_color_manual(values = blog_palette) +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
  scale_y_continuous(breaks = c(3.8e6, 4.2e6, 4.6e6),
                     labels = c("3.8", "4.2", "4.6")) +
  theme(axis.ticks.margin = unit(c(0, 0, 0.2, 0), "cm")) +
  labs(y = "Population (millions)")

grid.arrange(p.ghg.converted, p.population,
             heights = c(.75, .25), ncol = 1, nrow = 2)



# Read in the GNI figures
irish.gni <- 
  read.csv("Data_Extract_From_World_Development_Indicators_Data.csv")  
 
gni.vector <- t(irish.gni)[5:17]

converted.ghg.data$gni <- gni.vector

converted.ghg.data %<>% 
  mutate(gni = as.numeric(gni)) %>% 
  mutate(gni.per.capita = gni/ population)
  
# Get GNI per Capita and GHG per Capita plots
p.gni <-
  ggplot(converted.ghg.data, aes(Year, gni.per.capita)) +
  geom_line(size = 1.5) +
  test.theme +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
  labs(title = "Irish Gross National Income per Capita (2000 - 2012)",
       y = "GNI Per Capita ($)")

p.ghg.per.capita <-
  ggplot(filter(converted.ghg.data, category.short.names == "Total GHG's"), 
                aes(Year, ghg.emmissions.per.capita)) +
  geom_line(size = 1.5, colour = blog_palette[6]) +
  test.theme +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
  theme(axis.ticks.margin = unit(c(0, 0, 0.3, 0), "cm")) + # for lining up
  labs(title = "Irish Total GHG Emmissions per Capita (2000 - 2012)",
       y = "GNI Per Capita ($)")

# Plot in vertical arrangement
grid.arrange(p.gni, p.ghg.per.capita,
             heights = c(.5, .5), ncol = 1, nrow = 2)


# Ratio of GHG's to GNI
converted.ghg.data %<>% 
  mutate(GHG.gni.ratio = (Total.emissions * 1000) / gni,
         gni.GHG.ratio = gni / (Total.emissions * 1000))

ggplot(converted.ghg.data, aes(Year, GHG.gni.ratio, 
                               colour = category.short.names)) +
  geom_line(size = 1.5) +
  test.theme +
  scale_color_manual(values = blog_palette, name = "GHG Type") +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
  labs(title = "Irish Ratio of GHG Equivalent to GNI 2000 - 2012",
       y = "Ratio of GHG Equivalent to GNI (tons per dollar)") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)
