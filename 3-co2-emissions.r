#### Importing packages ####
rm(list=ls())
library(treemap)



#### Import data from existing CSV (https://ourworldindata.org/grapher/food-emissions-supply-chain?country=Beef+%28beef+herd%29~Cheese~Poultry+Meat~Milk~Eggs~Rice~Pig+Meat~Peas~Bananas~Fish+%28farmed%29~Lamb+%26+Mutton~Beef+%28dairy+herd%29~Shrimps+%28farmed%29~Tofu~Coffee~Sunflower+Oil~Olive+Oil~Palm+Oil~Dark+Chocolate~Tomatoes) ####
setwd("../data")
data <- read_csv("3-food-emissions-supply-chain.csv")



#### Data cleaning ####
# Creates a new column, that sum up all the different food emissions
data$food_emission_total = data$"Uso della terra" + data$"Coltivazione e/o allevamento"  + data$"Alimentazione di animali" + data$"Lavorazione" + data$"Trasporto" + data$"Vendita" + data$"Packaging" + data$"Perdite"

# Drop unnecessary columns
data <- subset(data, select = -c(Code,Year))



#### Treemap of different foods emissions ####
setwd("../graphs")
pdf('3 - Emissioni di CO2 equivalenti prodotte da un kg di prodotto.pdf', width = 10, height = 10)
par(mfrow=c(2, 2), mar=c(1,1,1,1))
treemap(data,
        index=c("Entity"),
        vSize = "food_emission_total",
        fontsize.labels = c(15),
        fontcolor.labels = "white",
        title = "Emissioni di CO2 equivalenti prodotte da un kg di prodotto",
        fontsize.title = 20
)

dev.off()

#### Interesting stats ####
meat_emissions = data$food_emission_total[data$Entity == "Manzo"] + data$food_emission_total[data$Entity == "Bovini"] + data$food_emission_total[data$Entity == "Agnello & Montone"]
total_emissions = sum(data$food_emission_total, na.rm = TRUE)
perc_meat_emissions = meat_emissions * 100 / total_emissions

#### Data cleaning for the second treemap ####

# I want to display in treemap graph only the different type of emissions of "Beef (beef herd)"

data <- data %>% filter(Entity == "Manzo")
data <- subset(data, select = -c(Entity))
data <- t(data)
colnames(data)[1] = "co2_emissions"
data=as.data.frame(data)
data <- tibble::rownames_to_column(data, "type_of_emissions")
data <- filter(data, type_of_emissions != "food_emission_total")
data$type_of_emissions[2] = "Allevamento"



#### Second treemap ####

setwd("../graphs")
pdf('4 - Emissioni di CO2 equivalenti prodotte da un kg di manzo.pdf', width = 10, height = 10)
par(mfrow=c(2, 2), mar=c(1,1,1,1))
treemap(data,
        index=c("type_of_emissions"),
        vSize = "co2_emissions",
        fontsize.labels = c(13),
        fontcolor.labels = "white",
        title = "Emissioni di CO2 equivalenti prodotte da un kg di manzo",
        fontsize.title = 20
)
dev.off()


#### Interesting stats #2 ####

total_beef_emissions = sum(data$co2_emissions, na.rm = TRUE)
beef_emissions_allevamento = data$co2_emissions[data$type_of_emissions == "Coltivazione e/o allevamento"]
beef_emissions_transport = data$co2_emissions[data$type_of_emissions == "Trasporto"]
perc_beef_emissions_allevamento = beef_emissions_allevamento * 100 / total_beef_emissions
perc_beef_emissions_transport = beef_emissions_transport * 100 / total_beef_emissions

