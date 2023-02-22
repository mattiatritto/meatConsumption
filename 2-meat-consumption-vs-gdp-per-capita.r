#### Importing packages ####
rm(list=ls())
library(gganimate)
library(tidyverse)
library(ggthemes)
library(dplyr)

#### Import data from existing CSV https://ourworldindata.org/grapher/meat-consumption-vs-gdp-per-capita ####
setwd("../data")
data <- read_csv("2-meat-consumption-vs-gdp-per-capita.csv")

#### Data cleaning ####
# Rename columns
colnames(data)[1] <- "name"
colnames(data)[2] <- "country_code"
colnames(data)[3] <- "year"
colnames(data)[4] <- "meat_kg_capita_yr"
colnames(data)[5] <- "gpd_capita_yr"
colnames(data)[6] <- "population"
colnames(data)[7] <- "continent"

# Because in the original dataset for each observation only when year = 2015 there is the continent, this piece of code copy for each country the continent, based on the value that has for year = 2015
rows_to_delete <- data %>% filter (is.na(continent) & year == 2015)
countries <- unique(rows_to_delete$name)
for (country in 1:26){
  data <- data %>% filter (!(name == countries[country]))
}
countries <- unique(data$name)
data$continent[is.na(data$continent)] <- ""
for (country in 1:285){
  continent <- data$continent[data$year == 2015 & data$name == countries[country]]
  data$continent[data$name == countries[country]] <- continent
}


# Filter data that have NA population, NA gdp_per_capita, meat_kg_capita
data <- data %>% filter (!is.na(population) & !is.na(gpd_capita_yr) & !is.na(meat_kg_capita_yr))


# Creates a label for diplsaying on the scatterplot
data['label_scatterplot'] <- ""


#data$gpd_capita_yr[data$name == "United States"] <- 1

# This is for displaying only labels of countries that have over 200M people

index = data$population > 200000000
data$label_scatterplot[index] <- data$name[index] 
data$label_scatterplot[data$name == "United States"] <- "USA"
data$label_scatterplot[data$name == "Italy"] <- "Italy"
data$label_scatterplot[data$name == "Germany"] <- "Germany"
data$label_scatterplot[data$name == "Spain"] <- "Spain"
data$label_scatterplot[data$name == "Australia"] <- "Australia"

data_2017 <- data %>% filter (data$"year" == 2017)


# Estimating correlation between GPD and meat consumption
data_corr <- data_2017
data_corr$meat_kg_capita_yr[is.na(data_corr$meat_kg_capita_yr)] <- 0
data_corr$gpd_capita_yr[is.na(data_corr$gpd_capita_yr)] <- 0
cor(data_corr$meat_kg_capita_yr, data_corr$gpd_capita_yr)



#### Scatterplot ####
# Builds a scatterplot based on ggplot() package
graph1 = data_2017 %>%
  ggplot(aes(x=gpd_capita_yr, y=meat_kg_capita_yr, color=continent, label=label_scatterplot, cex=population)) +
  geom_point(alpha = 0.8, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,30), guide="none") +
  scale_x_sqrt(labels = scales::dollar) +
  labs(title = "Consumo di carne VS PIL pro-capite per paese",
       subtitle = "Ogni pallino rappresenta una nazione. La dimensione del pallino è proporzionale alla popolazione.",
       x = "PIL pro-capite ($)",
       y = "Consumo di carne (kg / per persona all'anno)",
       color = "Continente") +
  theme(axis.title = element_text(),
        legend.text=element_text(size=10),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill="white")) +
  scale_color_brewer(palette = "Set2") +
  geom_text(check_overlap = FALSE, size=4, hjust = 0, nudge_x = 0.05)+
  expand_limits(x = 0, y = 0)+
  coord_cartesian(expand = TRUE)


# Save the result in PDF format
setwd("../graphs")
pdf('2 - Consumo di carne VS PIL pro-capite.pdf', width = 12, height = 8)
par(mfrow=c(2, 2), mar=c(1,1,1,1))
graph1
dev.off()




#### Data cleaning for dynamic scatterplot ####
data <- data %>% filter (data$"year" > 1960 & data$"year" < 2017 & label_scatterplot != "")

#### Second scatterplot ####
graph1 = data %>%
  ggplot(aes(x=gpd_capita_yr, y=meat_kg_capita_yr, color=continent, label=label_scatterplot, cex=population)) +
  geom_point(alpha = 0.9, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,30), guide="none") +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "Consumo di carne VS PIL pro-capite per paese",
       x = "PIL procapite ($)",
       y = "Consumo di carne (kg / capita)",
       color = "Continente",
       caption = "Figure 2, credits: Our World in Data") +
  theme(axis.title = element_text(),
        legend.text=element_text(size=10),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill="white")) +
  scale_color_brewer(palette = "Set2") +
  geom_text(check_overlap = FALSE, size=4, hjust = 0, nudge_x = 0.05)+
  expand_limits(x = 0, y = 0)+
  coord_cartesian(expand = TRUE)

graph1



#### Scatterplot animation through the years ####
graph1.animation = graph1 + transition_time(year) + labs(subtitle = "Year: {frame_time}")
animate(graph1.animation, height = 600, width = 900, duration = 10, res = 110)

