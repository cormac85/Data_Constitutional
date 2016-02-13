# Author: COrmac Nolan
# Date: 05/07/2015
# Title: Energy and Standards of Living


setwd("C:/Users/Nasum/Google Drive/Coding/R/1. Energy and Mortality")

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# Set up themes

# http://paletton.com/palette.php?uid=3070u0kllllaFw0g0qFqFg0w0aF
# reddish - bluish -  greenish

source("../Utilities/blog.theme.R")

# Data retrieved from: http://apps.who.int/gho/data/?theme=home
energy.data <- read.csv("eg.use.elec.kh.pc_Indicator_en_csv_v2.csv")
adult.mortality.data <- read.csv("adult_mortality_byCountry.csv")
female.workforce.participation <- read.csv("sl.tlf.cact.fe.zs_Indicator_en_csv_v2.csv")
country.groups <- read.csv("world_regions.csv", header=F)
sanitation.data <- read.csv("sanitation.csv")

# Get names in order
colnames(country.groups) <- c("country", "region")
colnames(female.workforce.participation)[1] <- "country"
colnames(sanitation.data)[1] <- "country"
colnames(sanitation.data)[2] <- "water_sources"
colnames(sanitation.data)[7] <- "sanitation"

# Select relevant columns and rows
female.workforce.participation <- select(female.workforce.participation, country, X2011)
female.workforce.participation <- na.omit(female.workforce.participation)

sanitation.data <- na.omit(select(sanitation.data, country, water_sources, sanitation))
sanitation.data <- separate(sanitation.data, country, sep="; ", into=c("country", "year"))
sanitation.data <- filter(sanitation.data, year == 2012 )

# Create the target dataset
data <- data.frame(energy.data$X2011)
data$country.name <- energy.data$ï..Country.Name

adult.mortality.data <- separate(adult.mortality.data, Country..Year, sep="; ", into=c("country", "year"))
colnames(adult.mortality.data)[3] <- "both"
colnames(adult.mortality.data)[4] <- "female"
colnames(adult.mortality.data)[5] <- "male"
colnames(data)[2] <- "country"
colnames(female.workforce.participation)[2] <- "female_participation"


adult.mortality.data.2012 <- filter(adult.mortality.data, year == 2012)

data <- inner_join(adult.mortality.data.2012, data, by="country" )
data$year <- NULL

data <- na.omit(data)

data <- inner_join(data, country.groups, by="country")
data <- inner_join(data, female.workforce.participation, by = "country")
data <- inner_join(data, sanitation.data, by="country")

data$year <- NULL


p.data.mortality.byRegion <- ggplot(data, aes(energy.data.X2011, both, colour=region))


p.data.mortality.byRegion +
  geom_point(stat="identity", size=5, alpha = 0.75)+
  geom_line(stat="smooth", method="loess", formula = y~x, 
            size =2, alpha = .75, span=.55, se=F, colour="black")+
  blog.theme+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"))+
  ggtitle("Electricity Consumption v Mortality")+
  xlab("Electricity Consumption (kWhr per Capita) - 2011")+
  ylab("Adult (15-60) Mortality Rate per Thousand - 2012")+
  scale_colour_manual(values = blog.palette)

# p.data.mortality <- ggplot(data, aes(energy.data.X2011, both))
# 
# p.data.mortality +
#   geom_point(stat="identity", size=5, alpha = 0.75)+
#   geom_line(stat="smooth", method="loess", formula = y~x, 
#             size =2, alpha = 0.8, span=.55, se=F, colour=blog.palette[1])+
#   blog.theme+
#   ggtitle("Electricity Consumption v Mortality")+
#   xlab("Electricity Consumption (kWhr per Capita) - 2011")+
#   ylab("Adult (15-60) Mortality Rate per Thousand - 2012")

# Female participation rate in the workforce.
# p.data.participation <- ggplot(data, aes(energy.data.X2011, female_participation))
# 
# p.data.participation +
#   geom_point(stat="identity", size=5, alpha = 0.75)+
#   geom_line(stat="smooth", method="loess", formula = y~x, 
#             size =2, alpha = 0.8, span=.55, se=F, colour=blog.palette[1])+
#   blog.theme+
#   ggtitle("Electricity Consumption v Female Workforce Participation")+
#   xlab("Electricity Consumption (kWhr per Capita) - 2011")+
#   ylab("Female Workforce Participation (%) - 2011")

# Access to clean water and sanitation
# Last thing needed is to "gather()" the sanitation and clean_water variables in order to 
# more easily plot them  with ggplot. Current attempt below is... messy...
p.data.sanitation<- ggplot(data, aes(energy.data.X2011))

p.data.sanitation +
  geom_point(aes(y=water_sources, colour="water_sources"),
             stat="identity", 
             size=3,
             alhpa = 0.75)+
  geom_point(aes(y=sanitation, colour="sanitation"),
             stat="identity", 
             size=3,
             alpha = 0.75)+
  geom_line(aes(y=water_sources), stat="smooth", method="loess", formula = y~x, 
            size =2, span=.5, se=F, colour = blog.palette[1])+
  geom_line(aes(y=sanitation), stat="smooth", method="loess", formula = y~x, 
            size =2, span=.5, se=F, colour = blog.palette[2])+
  blog.theme+
  ggtitle("Electricity Consumption v Sanitation Rates")+
  xlab("Electricity Consumption (kWhr per Capita) - 2011")+
  ylab("Access to Clean Water & Sanitation (%) - 2011")+
  scale_color_manual(values = blog.palette)