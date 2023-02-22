#### Importing packages ####
rm(list=ls())
library(ggplot2)
library(gganimate)
library(tidyverse)
library(dplyr)
library(viridisLite)
library(viridis)
library(ggthemes)



#### Import data from existing CSV (https://ourworldindata.org/grapher/meat-supply-per-person) ####
setwd("../data")
data <- read_csv("1-meat-supply-per-person.csv")


#### Data cleaning ####
# Rename columns
colnames(data)[1] <- "region"
colnames(data)[2] <- "country_code"
colnames(data)[3] <- "year"
colnames(data)[4] <- "meat_kg_capita_yr"



#Fiter only data in 2017
data_2017 <- data %>% filter (data$"year" == 2017)



#### Static choropleth graph ####

# mapdata includes information about displaying every country in the globe
mapdata <- map_data("world")
colnames(mapdata)[5] <- "region"

# Uniforming names in these two different datasets
mapdata$region <- as.character(mapdata$country_code)    
mapdata$region[mapdata$region == "USA"] <- "United States"    
mapdata$region[mapdata$region == "UK"] <- "United Kingdom"    
mapdata$region <- as.factor(mapdata$region)     

# We combine this dataset with our previous data
mapdata_2017 <- left_join(mapdata, data_2017, by="region")

#Checking the distribution of data, to decide which color scale to use (linear or logaritmic)
data_2017 %>% ggplot(aes(x=meat_kg_capita_yr)) + geom_histogram(bins=20) + scale_x_log10()

#Using ggplot() to display the Choropleth graph
map <- ggplot() + 
       geom_polygon(data = mapdata_2017, aes(fill=meat_kg_capita_yr, x=long, y=lat, group=group), size=0, alpha=0.9) +
       theme_map() +
       scale_fill_viridis(name="Kg di carne pro-capite consumati nel 2017",breaks=c(1,30,60,90,120), guide=guide_legend(keyheight=unit(3, units ="mm"), keywidth=unit(12, units ="mm"), label.position="bottom", title.position='top', nrow=1, na.value="gray")) +
       labs(
          title = "Quanta carne consuma in media una persona in un anno?",
          subtitle = "Il colore indica la quantità annuale di carne, in grigio sono rapprsentati i paesi di cui non sono disponibili dati."
        ) +
       theme(
          text = element_text(color="black"),
          plot.title = element_text(size=22, hjust=0.01, color="black", margin=margin(b=-0.1, t=0.5, l=5, r=4, unit="cm")),
          plot.subtitle = element_text(size=11, hjust=0.01, color="black", margin=margin(b =-0.1, t=0.8, l=5, r=4, unit="cm")),
          plot.caption = element_text(size=11, color="black", margin=margin(b=0.3, r=-99, unit="cm")),
          legend.position = c(0.7, 0.09)
       ) 

# Save the result in PDF format
setwd("../graphs")
pdf('1 - Quanta carne consuma in media una persona in un anno?.pdf', width = 12, height = 8)
par(mfrow=c(2, 2), mar=c(1,1,1,1))
map
dev.off()



#### Dynamic choropleth graph (animation through the years) ####

mapdata <- map_data("world")
colnames(mapdata)[5] <- "region"

# Uniforming names in these two different datasets
mapdata$region <- as.character(mapdata$country_code)    
mapdata$region[mapdata$region == "USA"] <- "United States"    
mapdata$region <- as.factor(mapdata$region)   
mapdata <- left_join(mapdata, data, by="region")

# Create the base map
map <- ggplot() + 
  geom_polygon(data=mapdata, aes(fill=meat_kg_capita_yr, x=long, y=lat, group=group) , size=0, alpha=0.9) +
  theme_map() +
  scale_fill_viridis(name="Kg procapite consumati", breaks=c(1,30,60,90,120), guide=guide_legend(keyheight=unit(3, units="mm"), keywidth=unit(12, units ="mm"), label.position = "bottom", title.position ='top', nrow=1, na.value="gray")) +
  labs(
    title = "Quanta carne consuma in media una persona in un anno?",
    caption = "Figura 1, credits: Our World in Data"
  ) +
  theme(
    text = element_text(color="black"),
    plot.title = element_text(size=18, hjust=0.01, color="black", margin=margin(b=-0.1, t=0.5, l=5, r=4, unit="cm")),
    plot.subtitle = element_text(size=11, hjust=0.01, color="black", margin=margin(b =-0.1, t=0.8, l=5, r=4, unit="cm")),
    plot.caption = element_text(size=11, color="black", margin=margin(b=0.3, r=-99, unit="cm")),
    legend.position = c(0.7, 0.09)
  ) 


# Creates the animation through the years
map.animation = map + transition_time(year)+ labs(subtitle = "Anno {frame_time}") + shadow_wake(wake_length=0.1)

map_with_animation <- map + transition_time(year) + ggtitle('Quanta carne consuma in media una persona in un anno? Anno {frame_time}')

num_years = max(data$year) - min(data$year) + 1
animate(map_with_animation, nframes = num_years, height = 600, width = 900, duration = 10, res = 110)



#### Interesting stats ####

# Percentage increase per capita in meat consumption from 1961 to 2017

data_1961 <- data %>% filter (data$"year" == 1961)
data_2017 <- data %>% filter (data$"year" == 2017)

meat_avg_1961 = mean(data_1961$meat_kg_capita_yr)
meat_avg_2017 = mean(data_2017$meat_kg_capita_yr)

increase_meat_percentage = (meat_avg_2017 - meat_avg_1961) * 100 /meat_avg_1961


# For every country, I want to estimate the increase percentage in meat consumption from 1961 to 2017

data_1961 <- data %>% filter (data$"year" == 1961)
data_2017 <- data %>% filter (data$"year" == 2017)

colnames(data_1961)[4] <- "meat_kg_capita_yr_1961"
colnames(data_2017)[4] <- "meat_kg_capita_yr_2017"

increase_percentage <- full_join(data_1961, data_2017, by="region")
increase_percentage$increase_percentage_meat_consumption = (increase_percentage$meat_kg_capita_yr_2017 - increase_percentage$meat_kg_capita_yr_1961)*100 / increase_percentage$meat_kg_capita_yr_1961 

increase_percentage = subset(increase_percentage, select = -c(country_code.x,country_code.y, year.x, year.y))
increase_percentage <- increase_percentage %>% filter(!is.na(increase_percentage$"increase_percentage_meat_consumption"))

