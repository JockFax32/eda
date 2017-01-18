# Exploratory data analysis (see readme for more detail)
# Feel free to follow these steps, or complete your own EDA

# Set up (install packages if you don't have them)
setwd("~/Desktop/School/University_Washington/2016-2017/Winter/INFO 370/Modules/EDA/eda/health-burden/data/")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(vioplot)
risk.data <- read.csv('prepped/risk-data.csv', stringsAsFactors = FALSE) 
risk.data <- as.data.frame(risk.data)

######################
### Data Structure ###
######################

## Using a variety of functions, investigate the structure of your data, such as:

# Dimensions, column names, structure, summaries, etc.
num.col <- ncol(risk.data)
num.row <- nrow(risk.data)
col.names <- colnames(risk.data)

summary(risk.data, na.rm = TRUE)

# Replacing missing values...?
# new.risk <- na.omit(risk.data)
risk.data[is.na(risk.data)] <- 0

###########################
### Univariate Analysis ###
###########################

## Using a variety of approaches, investigate the structure each (risk column) individually
str(risk.data)

# Summarize data
lapply(risk.data, summary)

# Create histograms, violin plots, boxplots
hist(risk.data$alcohol.use)
hist(risk.data$low.exercise)
hist(risk.data$high.meat)
####################################
### Univariate Analysis (by age) ###
####################################

# Investiage how each risk-variable varies by **age group**
risk.data <- risk.data %>% 
  group_by(age) %>% 
  summarise(risk.data$high.meat)

# Create histograms, violin plots, boxplots. Calculate values as needed. 
boxplot(high.meat ~ age, data = risk.data, las=2)
boxplot(drug.use ~ age, data = risk.data, las=2)
boxplot(low.exercise ~ age, data = risk.data, las=2)


####################################
### Univariate Analysis (by sex) ###
####################################

# Investiage how each risk-variable varies by **sex**


# Compare male to female values -- requires reshaping (and dropping population)!


########################################
### Univariate Analysis (by country) ###
########################################

## Investiage how each risk-variable varies by **country**
# Aggregate by country
by.country <- risk.data %>%
  group_by(country, country.code) %>% 		
  summarize(smoking = sum(smoking * pop), pop=sum(pop)) %>% 		
  ungroup() %>% 		
  mutate(smoking.rate = smoking / pop)		


# Create a choropleth map (see https://plot.ly/r/choropleth-maps/)


###########################
### Bivariate Analysis ####
###########################

# Compare risks-variables to one another (visually)
