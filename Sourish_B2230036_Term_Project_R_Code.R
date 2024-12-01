# Time Series Forecasting

# Installing the required packages
#install.packages("curl")
#install.packages("TTR")
#install.packages("quantmod")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("ggplot2")


library("forecast")
library("ggplot2")
library("tseries")
library("zoo")

# Loading the dataset
data <- read.csv("All_India_CPI_upto_March_2023.csv", header = T)
head(data)
str(data)
summary(data) # Getting the summary of the Data



View(data)

# Converting to univariate series

# CPI(Cereals and products)
data_CandP_Rural = ts(as.numeric(data$Cereals.and.products[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_CandP_Rural)
head(data_CandP_Rural)
summary(data_CandP_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_CandP_Rural)) # The output is 88 here
data_CandP_Rural[88] = sum(data_CandP_Rural[86:87], data_CandP_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_CandP_Urban = ts(as.numeric(data$Cereals.and.products[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_CandP_Urban)
head(data_CandP_Urban)
summary(data_CandP_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_CandP_Urban)) # The output is 88 here
data_CandP_Urban[88] = sum(data_CandP_Urban[86:87], data_CandP_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_CandP_Both = ts(as.numeric(data$Cereals.and.products[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_CandP_Both)
head(data_CandP_Both)
summary(data_CandP_Both) # Getting the summary of the data_Egg_Urban
which(is.na(data_CandP_Both)) # The output is 88 here
data_CandP_Both[88] = sum(data_CandP_Both[86:87], data_CandP_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Meat and fish)
data_MandF_Rural = ts(as.numeric(data$Meat.and.fish[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_MandF_Rural)
head(data_MandF_Rural)
summary(data_MandF_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_MandF_Rural)) # The output is 87, 88 here
data_MandF_Rural[88] = sum(data_MandF_Rural[86], data_MandF_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_MandF_Rural[87] = sum(data_MandF_Rural[85:86], data_MandF_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_MandF_Urban = ts(as.numeric(data$Meat.and.fish[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_MandF_Urban)
head(data_MandF_Urban)
summary(data_MandF_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_MandF_Urban)) # The output is 87, 88 here
data_MandF_Urban[88] = sum(data_MandF_Urban[86], data_MandF_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_MandF_Urban[87] = sum(data_MandF_Urban[85:86], data_MandF_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_MandF_Both = ts(as.numeric(data$Meat.and.fish[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_MandF_Both)
head(data_MandF_Both)
summary(data_MandF_Both) # Getting the summary of the data_Egg_Urban
which(is.na(data_MandF_Both)) # The output is 87, 88 here
data_MandF_Both[88] = sum(data_MandF_Both[86], data_MandF_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_MandF_Both[87] = sum(data_MandF_Both[85:86], data_MandF_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Egg)
data_Egg_Rural = ts(as.numeric(data$Egg[data$Sector=='Rural']), start = c(2013,1), 
                    frequency = 12)
str(data_Egg_Rural)
head(data_Egg_Rural)
summary(data_Egg_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Egg_Rural)) # The output is 88 here
data_Egg_Rural[88] = sum(data_Egg_Rural[86:87], data_Egg_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Egg_Urban = ts(as.numeric(data$Egg[data$Sector=='Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Egg_Urban)
head(data_Egg_Urban)
summary(data_Egg_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Egg_Urban)) # The output is 88 here
data_Egg_Urban[88] = sum(data_Egg_Urban[86:87], data_Egg_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Egg_Both = ts(as.numeric(data$Egg[data$Sector=='Rural+Urban']), start = c(2013,1), 
                   frequency = 12)
str(data_Egg_Both)
head(data_Egg_Both)
summary(data_Egg_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Egg_Both)) # The output is 88 here
data_Egg_Both[88] = sum(data_Egg_Both[86:87], data_Egg_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Milk and products)
data_Milk_Rural = ts(as.numeric(data$Milk.and.products[data$Sector=='Rural']), start = c(2013,1), 
                     frequency = 12)
str(data_Milk_Rural)
head(data_Milk_Rural)
summary(data_Milk_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Milk_Rural)) # The output is 88 here
data_Milk_Rural[88] = sum(data_Milk_Rural[86:87], data_Milk_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Milk_Urban = ts(as.numeric(data$Milk.and.products[data$Sector=='Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Milk_Urban)
head(data_Milk_Urban)
summary(data_Milk_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Milk_Urban)) # The output is 88 here
data_Milk_Urban[88] = sum(data_Milk_Urban[86:87], data_Milk_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Milk_Both = ts(as.numeric(data$Milk.and.products[data$Sector=='Rural+Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Milk_Both)
head(data_Milk_Both)
summary(data_Milk_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Milk_Both)) # The output is 88 here
data_Milk_Both[88] = sum(data_Milk_Both[86:87], data_Milk_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Oil and fats)
data_Oil_Rural = ts(as.numeric(data$Oils.and.fats[data$Sector=='Rural']), start = c(2013,1), 
                    frequency = 12)
str(data_Oil_Rural)
head(data_Oil_Rural)
summary(data_Oil_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Oil_Rural)) # The output is 88 here
data_Oil_Rural[88] = sum(data_Oil_Rural[86:87], data_Oil_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Oil_Urban = ts(as.numeric(data$Oils.and.fats[data$Sector=='Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Oil_Urban)
head(data_Oil_Urban)
summary(data_Oil_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Oil_Urban)) # The output is 88 here
data_Oil_Urban[88] = sum(data_Oil_Urban[86:87], data_Oil_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Oil_Both = ts(as.numeric(data$Oils.and.fats[data$Sector=='Rural+Urban']), start = c(2013,1), 
                   frequency = 12)
str(data_Oil_Both)
head(data_Oil_Both)
summary(data_Oil_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Oil_Both)) # The output is 88 here
data_Oil_Both[88] = sum(data_Oil_Both[86:87], data_Oil_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Fruits)
data_Fruits_Rural = ts(as.numeric(data$Fruits[data$Sector=='Rural']), start = c(2013,1), 
                       frequency = 12)
str(data_Fruits_Rural)
head(data_Fruits_Rural)
summary(data_Fruits_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Fruits_Rural)) # The output is 88 here
data_Fruits_Rural[88] = sum(data_Fruits_Rural[86:87], data_Fruits_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Fruits_Urban = ts(as.numeric(data$Fruits[data$Sector=='Urban']), start = c(2013,1), 
                       frequency = 12)
str(data_Fruits_Urban)
head(data_Fruits_Urban)
summary(data_Fruits_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Fruits_Urban)) # The output is 88 here
data_Fruits_Urban[88] = sum(data_Fruits_Urban[86:87], data_Fruits_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Fruits_Both = ts(as.numeric(data$Fruits[data$Sector=='Rural+Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Fruits_Both)
head(data_Fruits_Both)
summary(data_Fruits_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Fruits_Both)) # The output is 88 here
data_Fruits_Both[88] = sum(data_Fruits_Both[86:87], data_Fruits_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Vegetables)
data_Veg_Rural = ts(as.numeric(data$Vegetables[data$Sector=='Rural']), start = c(2013,1), 
                    frequency = 12)
str(data_Veg_Rural)
head(data_Veg_Rural)
summary(data_Veg_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Veg_Rural)) # The output is 88 here
data_Veg_Rural[88] = sum(data_Veg_Rural[86:87], data_Veg_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Veg_Urban = ts(as.numeric(data$Vegetables[data$Sector=='Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Veg_Urban)
head(data_Veg_Urban)
summary(data_Veg_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Veg_Urban)) # The output is 88 here
data_Veg_Urban[88] = sum(data_Veg_Urban[86:87], data_Veg_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Veg_Both = ts(as.numeric(data$Vegetables[data$Sector=='Rural+Urban']), start = c(2013,1), 
                   frequency = 12)
str(data_Veg_Both)
head(data_Veg_Both)
summary(data_Veg_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Veg_Both)) # The output is 88 here
data_Veg_Both[88] = sum(data_Veg_Both[86:87], data_Veg_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Pulses and products)
data_Pulses_Rural = ts(as.numeric(data$Pulses.and.products[data$Sector=='Rural']), start = c(2013,1), 
                       frequency = 12)
str(data_Pulses_Rural)
head(data_Pulses_Rural)
summary(data_Pulses_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Pulses_Rural)) # The output is 88 here
data_Pulses_Rural[88] = sum(data_Pulses_Rural[86:87], data_Pulses_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Pulses_Urban = ts(as.numeric(data$Pulses.and.products[data$Sector=='Urban']), start = c(2013,1), 
                       frequency = 12)
str(data_Pulses_Urban)
head(data_Pulses_Urban)
summary(data_Pulses_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Pulses_Urban)) # The output is 88 here
data_Pulses_Urban[88] = sum(data_Pulses_Urban[86:87], data_Pulses_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Pulses_Both = ts(as.numeric(data$Pulses.and.products[data$Sector=='Rural+Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Pulses_Both)
head(data_Pulses_Both)
summary(data_Pulses_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Pulses_Both)) # The output is 88 here
data_Pulses_Both[88] = sum(data_Pulses_Both[86:87], data_Pulses_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Sugar and Confectionery)
data_Suger_Rural = ts(as.numeric(data$Sugar.and.Confectionery[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_Suger_Rural)
head(data_Suger_Rural)
summary(data_Suger_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Suger_Rural)) # The output is 88 here
data_Suger_Rural[88] = sum(data_Suger_Rural[86:87], data_Suger_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Suger_Urban = ts(as.numeric(data$Sugar.and.Confectionery[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Suger_Urban)
head(data_Suger_Urban)
summary(data_Suger_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Suger_Urban)) # The output is 88 here
data_Suger_Urban[88] = sum(data_Suger_Urban[86:87], data_Suger_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Suger_Both = ts(as.numeric(data$Sugar.and.Confectionery[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Suger_Both)
head(data_Suger_Both)
summary(data_Suger_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Suger_Both)) # The output is 88 here
data_Suger_Both[88] = sum(data_Suger_Both[86:87], data_Suger_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Spices)
data_Spices_Rural = ts(as.numeric(data$Spices[data$Sector=='Rural']), start = c(2013,1), 
                       frequency = 12)
str(data_Spices_Rural)
head(data_Spices_Rural)
summary(data_Spices_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Spices_Rural)) # The output is 88 here
data_Spices_Rural[88] = sum(data_Spices_Rural[86:87], data_Spices_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Spices_Urban = ts(as.numeric(data$Sugar.and.Confectionery[data$Sector=='Urban']), start = c(2013,1), 
                       frequency = 12)
str(data_Spices_Urban)
head(data_Spices_Urban)
summary(data_Spices_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Spices_Urban)) # The output is 88 here
data_Spices_Urban[88] = sum(data_Spices_Urban[86:87], data_Spices_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Spices_Both = ts(as.numeric(data$Spices[data$Sector=='Rural+Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Spices_Both)
head(data_Spices_Both)
summary(data_Spices_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Spices_Both)) # The output is 88 here
data_Spices_Both[88] = sum(data_Spices_Both[86:87], data_Spices_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Non-Alcoholic Beverages)
data_NonAlcoho_Rural = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Rural']), start = c(2013,1), 
                          frequency = 12)
str(data_NonAlcoho_Rural)
head(data_NonAlcoho_Rural)
summary(data_NonAlcoho_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_NonAlcoho_Rural)) # The output is 88 here
data_NonAlcoho_Rural[88] = sum(data_NonAlcoho_Rural[86:87], data_NonAlcoho_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_NonAlcoho_Urban = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Urban']), start = c(2013,1), 
                          frequency = 12)
str(data_NonAlcoho_Urban)
head(data_NonAlcoho_Urban)
summary(data_NonAlcoho_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_NonAlcoho_Urban)) # The output is 88 here
data_NonAlcoho_Urban[88] = sum(data_NonAlcoho_Urban[86:87], data_NonAlcoho_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_NonAlcoho_Both = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Rural+Urban']), start = c(2013,1), 
                         frequency = 12)
str(data_NonAlcoho_Both)
head(data_NonAlcoho_Both)
summary(data_NonAlcoho_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_NonAlcoho_Both)) # The output is 88 here
data_NonAlcoho_Both[88] = sum(data_NonAlcoho_Both[86:87], data_NonAlcoho_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Non-Alcoholic Beverages)
data_NonAlcoho_Rural = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Rural']), start = c(2013,1), 
                          frequency = 12)
str(data_NonAlcoho_Rural)
head(data_NonAlcoho_Rural)
summary(data_NonAlcoho_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_NonAlcoho_Rural)) # The output is 88 here
data_NonAlcoho_Rural[88] = sum(data_NonAlcoho_Rural[86:87], data_NonAlcoho_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_NonAlcoho_Urban = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Urban']), start = c(2013,1), 
                          frequency = 12)
str(data_NonAlcoho_Urban)
head(data_NonAlcoho_Urban)
summary(data_NonAlcoho_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_NonAlcoho_Urban)) # The output is 88 here
data_NonAlcoho_Urban[88] = sum(data_NonAlcoho_Urban[86:87], data_NonAlcoho_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_NonAlcoho_Both = ts(as.numeric(data$Non.alcoholic.beverages[data$Sector=='Rural+Urban']), start = c(2013,1), 
                         frequency = 12)
str(data_NonAlcoho_Both)
head(data_NonAlcoho_Both)
summary(data_NonAlcoho_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_NonAlcoho_Both)) # The output is 88 here
data_NonAlcoho_Both[88] = sum(data_NonAlcoho_Both[86:87], data_NonAlcoho_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Prepared Meals, Snacks, Sweets)
data_Meals_Rural = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_Meals_Rural)
head(data_Meals_Rural)
summary(data_Meals_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Meals_Rural)) # The output is 87, 88 here
data_Meals_Rural[88] = sum(data_Meals_Rural[86], data_Meals_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Rural[87] = sum(data_Meals_Rural[85:86], data_Meals_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Meals_Urban = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Meals_Urban)
head(data_Meals_Urban)
summary(data_Meals_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Meals_Urban)) # The output is 87, 88 here
data_Meals_Urban[88] = sum(data_Meals_Urban[86], data_Meals_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Urban[87] = sum(data_Meals_Urban[85:86], data_Meals_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Meals_Both = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Meals_Both)
head(data_Meals_Both)
summary(data_Meals_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Meals_Both)) # The output is 88 here
data_Meals_Both[88] = sum(data_Meals_Both[86], data_Meals_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Both[87] = sum(data_Meals_Both[85:86], data_Meals_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Prepared Meals, Snacks, Sweets)
data_Meals_Rural = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_Meals_Rural)
head(data_Meals_Rural)
summary(data_Meals_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Meals_Rural)) # The output is 87, 88 here
data_Meals_Rural[88] = sum(data_Meals_Rural[86], data_Meals_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Rural[87] = sum(data_Meals_Rural[85:86], data_Meals_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Meals_Urban = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Meals_Urban)
head(data_Meals_Urban)
summary(data_Meals_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Meals_Urban)) # The output is 87, 88 here
data_Meals_Urban[88] = sum(data_Meals_Urban[86], data_Meals_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Urban[87] = sum(data_Meals_Urban[85:86], data_Meals_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Meals_Both = ts(as.numeric(data$Prepared.meals..snacks..sweets.etc.[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Meals_Both)
head(data_Meals_Both)
summary(data_Meals_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Meals_Both)) # The output is 88 here
data_Meals_Both[88] = sum(data_Meals_Both[86], data_Meals_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Meals_Both[87] = sum(data_Meals_Both[85:86], data_Meals_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Food and Beverages)
data_Food_Rural = ts(as.numeric(data$Food.and.beverages[data$Sector=='Rural']), start = c(2013,1), 
                     frequency = 12)
str(data_Food_Rural)
head(data_Food_Rural)
summary(data_Food_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Food_Rural)) # The output is 88 here
data_Food_Rural[88] = sum(data_Food_Rural[86:87], data_Food_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Food_Urban = ts(as.numeric(data$Food.and.beverages[data$Sector=='Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Food_Urban)
head(data_Food_Urban)
summary(data_Food_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Food_Urban)) # The output is 88 here
data_Food_Urban[88] = sum(data_Food_Urban[86:87], data_Food_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Food_Both = ts(as.numeric(data$Food.and.beverages[data$Sector=='Rural+Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Food_Both)
head(data_Food_Both)
summary(data_Food_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Food_Both)) # The output is 88 here
data_Food_Both[88] = sum(data_Food_Both[86:87], data_Food_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Pan, Tobacco and Intoxicants)
data_Pan_Rural = ts(as.numeric(data$Pan..tobacco.and.intoxicants[data$Sector=='Rural']), start = c(2013,1), 
                    frequency = 12)
str(data_Pan_Rural)
head(data_Pan_Rural)
summary(data_Pan_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Pan_Rural)) # The output is 87, 88 here
data_Pan_Rural[88] = sum(data_Pan_Rural[86], data_Pan_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Pan_Rural[87] = sum(data_Pan_Rural[85:86], data_Pan_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Pan_Urban = ts(as.numeric(data$Pan..tobacco.and.intoxicants[data$Sector=='Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Pan_Urban)
head(data_Pan_Urban)
summary(data_Pan_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Pan_Urban)) # The output is 87, 88 here
data_Pan_Urban[88] = sum(data_Pan_Urban[86], data_Pan_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Pan_Urban[87] = sum(data_Pan_Urban[85:86], data_Pan_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Pan_Both = ts(as.numeric(data$Pan..tobacco.and.intoxicants[data$Sector=='Rural+Urban']), start = c(2013,1), 
                   frequency = 12)
str(data_Pan_Both)
head(data_Pan_Both)
summary(data_Pan_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Pan_Both)) # The output is 88 here
data_Pan_Both[88] = sum(data_Pan_Both[86], data_Pan_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Pan_Both[87] = sum(data_Pan_Both[85:86], data_Pan_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Clothing)
data_Cloth_Rural = ts(as.numeric(data$Clothing[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_Cloth_Rural)
head(data_Cloth_Rural)
summary(data_Cloth_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Cloth_Rural)) # The output is 87, 88 here
data_Cloth_Rural[88] = sum(data_Cloth_Rural[86], data_Cloth_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Cloth_Rural[87] = sum(data_Cloth_Rural[85:86], data_Cloth_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Cloth_Urban = ts(as.numeric(data$Clothing[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Cloth_Urban)
head(data_Cloth_Urban)
summary(data_Cloth_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Cloth_Urban)) # The output is 87, 88 here
data_Cloth_Urban[88] = sum(data_Cloth_Urban[86], data_Cloth_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Cloth_Urban[87] = sum(data_Cloth_Urban[85:86], data_Cloth_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Cloth_Both = ts(as.numeric(data$Clothing[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Cloth_Both)
head(data_Cloth_Both)
summary(data_Cloth_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Cloth_Both)) # The output is 88 here
data_Cloth_Both[88] = sum(data_Cloth_Both[86], data_Cloth_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Cloth_Both[87] = sum(data_Cloth_Both[85:86], data_Cloth_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Footwear)
data_Foot_Rural = ts(as.numeric(data$Footwear[data$Sector=='Rural']), start = c(2013,1), 
                     frequency = 12)
str(data_Foot_Rural)
head(data_Foot_Rural)
summary(data_Foot_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Foot_Rural)) # The output is 87, 88 here
data_Foot_Rural[88] = sum(data_Foot_Rural[86], data_Foot_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Foot_Rural[87] = sum(data_Foot_Rural[85:86], data_Foot_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Foot_Urban = ts(as.numeric(data$Footwear[data$Sector=='Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Foot_Urban)
head(data_Foot_Urban)
summary(data_Foot_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Foot_Urban)) # The output is 87, 88 here
data_Foot_Urban[88] = sum(data_Foot_Urban[86], data_Foot_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Foot_Urban[87] = sum(data_Foot_Urban[85:86], data_Foot_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Foot_Both = ts(as.numeric(data$Footwear[data$Sector=='Rural+Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Foot_Both)
head(data_Foot_Both)
summary(data_Foot_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Foot_Both)) # The output is 88 here
data_Foot_Both[88] = sum(data_Foot_Both[86], data_Foot_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Foot_Both[87] = sum(data_Foot_Both[85:86], data_Foot_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Clothing and Footwear)
data_CandF_Rural = ts(as.numeric(data$Clothing.and.footwear[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_CandF_Rural)
head(data_CandF_Rural)
summary(data_CandF_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_CandF_Rural)) # The output is 87, 88 here
data_CandF_Rural[88] = sum(data_CandF_Rural[86], data_CandF_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_CandF_Rural[87] = sum(data_CandF_Rural[85:86], data_CandF_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_CandF_Urban = ts(as.numeric(data$Clothing.and.footwear[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_CandF_Urban)
head(data_CandF_Urban)
summary(data_CandF_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_CandF_Urban)) # The output is 87, 88 here
data_CandF_Urban[88] = sum(data_CandF_Urban[86], data_CandF_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_CandF_Urban[87] = sum(data_CandF_Urban[85:86], data_CandF_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_CandF_Both = ts(as.numeric(data$Clothing.and.footwear[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_CandF_Both)
head(data_CandF_Both)
summary(data_CandF_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_CandF_Both)) # The output is 88 here
data_CandF_Both[88] = sum(data_CandF_Both[86], data_CandF_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_CandF_Both[87] = sum(data_CandF_Both[85:86], data_CandF_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Housing)
# Since there is no data for Rural area of Housing CPI
data_House_Urban = ts(as.numeric(data$Housing[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_House_Urban)
head(data_House_Urban)
summary(data_House_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_House_Urban)) # The output is 88 here
data_House_Urban[88] = sum(data_House_Urban[86:87], data_House_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_House_Both = ts(as.numeric(data$Clothing.and.footwear[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_House_Both)
head(data_House_Both)
summary(data_House_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_House_Both)) # The output is 88 here
data_House_Both[88] = sum(data_House_Both[86], data_House_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_House_Both[87] = sum(data_House_Both[85:86], data_House_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Fuel and Light)
data_Fuel_Rural = ts(as.numeric(data$Fuel.and.light[data$Sector=='Rural']), start = c(2013,1), 
                     frequency = 12)
str(data_Fuel_Rural)
head(data_Fuel_Rural)
summary(data_Fuel_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Fuel_Rural)) # The output is 88 here
data_Fuel_Rural[88] = sum(data_Fuel_Rural[86:87], data_Fuel_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Fuel_Urban = ts(as.numeric(data$Fuel.and.light[data$Sector=='Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_Fuel_Urban)
head(data_Fuel_Urban)
summary(data_Fuel_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Fuel_Urban)) # The output is 88 here
data_Fuel_Urban[88] = sum(data_Fuel_Urban[86:87], data_Fuel_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Fuel_Both = ts(as.numeric(data$Fuel.and.light[data$Sector=='Rural+Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Fuel_Both)
head(data_Fuel_Both)
summary(data_Fuel_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Fuel_Both)) # The output is 88 here
data_Fuel_Both[88] = sum(data_Fuel_Both[86:87], data_Fuel_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Household Goods and Services)
data_Household_Rural = ts(as.numeric(data$Household.goods.and.services[data$Sector=='Rural']), start = c(2013,1), 
                          frequency = 12)
str(data_Household_Rural)
head(data_Household_Rural)
summary(data_Household_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Household_Rural)) # The output is 87, 88 here
data_Household_Rural[88] = sum(data_Household_Rural[86], data_Household_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Household_Rural[87] = sum(data_Household_Rural[85:86], data_Household_Rural[89])/3 # Manually replacing the NA value with neighborhood averages


data_Household_Urban = ts(as.numeric(data$Household.goods.and.services[data$Sector=='Urban']), start = c(2013,1), 
                          frequency = 12)
str(data_Household_Urban)
head(data_Household_Urban)
summary(data_Household_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Household_Urban)) # The output is 87, 88 here
data_Household_Urban[88] = sum(data_Household_Urban[86], data_Household_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Household_Urban[87] = sum(data_Household_Urban[85:86], data_Household_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Household_Both = ts(as.numeric(data$Household.goods.and.services[data$Sector=='Rural+Urban']), start = c(2013,1), 
                         frequency = 12)
str(data_Household_Both)
head(data_Household_Both)
summary(data_Household_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Household_Both)) # The output is 87, 88 here
data_Household_Both[88] = sum(data_Household_Both[86], data_Household_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Household_Both[87] = sum(data_Household_Both[85:86], data_Household_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Health)
data_Health_Rural = ts(as.numeric(data$Health[data$Sector=='Rural']), start = c(2013,1), 
                       frequency = 12)
str(data_Health_Rural)
head(data_Health_Rural)
summary(data_Health_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Health_Rural)) # The output is 88 here
data_Health_Rural[88] = sum(data_Health_Rural[86:87], data_Health_Rural[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Health_Urban = ts(as.numeric(data$Health[data$Sector=='Urban']), start = c(2013,1), 
                       frequency = 12)
str(data_Health_Urban)
head(data_Health_Urban)
summary(data_Health_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Health_Urban)) # The output is 88 here
data_Health_Urban[88] = sum(data_Health_Urban[86:87], data_Health_Urban[89:90])/4 # Manually replacing the NA value with neighborhood averages

data_Health_Both = ts(as.numeric(data$Health[data$Sector=='Rural+Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_Health_Both)
head(data_Health_Both)
summary(data_Health_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Health_Both)) # The output is 88 here
data_Health_Both[88] = sum(data_Health_Both[86:87], data_Health_Both[89:90])/4 # Manually replacing the NA value with neighborhood averages

# CPI(Transport and Communication)
data_TandC_Rural = ts(as.numeric(data$Transport.and.communication[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_TandC_Rural)
head(data_TandC_Rural)
summary(data_TandC_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_TandC_Rural)) # The output is 87, 88 here
data_TandC_Rural[88] = sum(data_TandC_Rural[86], data_TandC_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_TandC_Rural[87] = sum(data_TandC_Rural[85:86], data_TandC_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_TandC_Urban = ts(as.numeric(data$Transport.and.communication[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_TandC_Urban)
head(data_TandC_Urban)
summary(data_TandC_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_TandC_Urban)) # The output is 87, 88 here
data_TandC_Urban[88] = sum(data_TandC_Urban[86], data_TandC_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_TandC_Urban[87] = sum(data_TandC_Urban[85:86], data_TandC_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_TandC_Both = ts(as.numeric(data$Transport.and.communication[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_TandC_Both)
head(data_TandC_Both)
summary(data_TandC_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_TandC_Both)) # The output is 87, 88 here
data_TandC_Both[88] = sum(data_TandC_Both[86], data_TandC_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_TandC_Both[87] = sum(data_TandC_Both[85:86], data_TandC_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Recreation and Amusement)
data_RandA_Rural = ts(as.numeric(data$Recreation.and.amusement[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_RandA_Rural)
head(data_RandA_Rural)
summary(data_RandA_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_RandA_Rural)) # The output is 87, 88 here
data_RandA_Rural[88] = sum(data_RandA_Rural[86], data_RandA_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_RandA_Rural[87] = sum(data_RandA_Rural[85:86], data_RandA_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_RandA_Urban = ts(as.numeric(data$Recreation.and.amusement[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_RandA_Urban)
head(data_RandA_Urban)
summary(data_RandA_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_RandA_Urban)) # The output is 87, 88 here
data_RandA_Urban[88] = sum(data_RandA_Urban[86], data_RandA_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_RandA_Urban[87] = sum(data_RandA_Urban[85:86], data_RandA_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_RandA_Both = ts(as.numeric(data$Recreation.and.amusement[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_RandA_Both)
head(data_RandA_Both)
summary(data_RandA_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_RandA_Both)) # The output is 87, 88 here
data_RandA_Both[88] = sum(data_RandA_Both[86], data_RandA_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_RandA_Both[87] = sum(data_RandA_Both[85:86], data_RandA_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Education)
data_Education_Rural = ts(as.numeric(data$Education[data$Sector=='Rural']), start = c(2013,1), 
                          frequency = 12)
str(data_Education_Rural)
head(data_Education_Rural)
summary(data_Education_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Education_Rural)) # The output is 87, 88 here
data_Education_Rural[88] = sum(data_Education_Rural[86], data_Education_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Education_Rural[87] = sum(data_Education_Rural[85:86], data_Education_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Education_Urban = ts(as.numeric(data$Education[data$Sector=='Urban']), start = c(2013,1), 
                          frequency = 12)
str(data_Education_Urban)
head(data_Education_Urban)
summary(data_Education_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Education_Urban)) # The output is 87, 88 here
data_Education_Urban[88] = sum(data_Education_Urban[86], data_Education_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Education_Urban[87] = sum(data_Education_Urban[85:86], data_Education_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Education_Both = ts(as.numeric(data$Education[data$Sector=='Rural+Urban']), start = c(2013,1), 
                         frequency = 12)
str(data_Education_Both)
head(data_Education_Both)
summary(data_Education_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Education_Both)) # The output is 87, 88 here
data_Education_Both[88] = sum(data_Education_Both[86], data_Education_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Education_Both[87] = sum(data_Education_Both[85:86], data_Education_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Personal Care and Effects)
data_PCare_Rural = ts(as.numeric(data$Personal.care.and.effects[data$Sector=='Rural']), start = c(2013,1), 
                      frequency = 12)
str(data_PCare_Rural)
head(data_PCare_Rural)
summary(data_PCare_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_PCare_Rural)) # The output is 87, 88 here
data_PCare_Rural[88] = sum(data_PCare_Rural[86], data_PCare_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_PCare_Rural[87] = sum(data_PCare_Rural[85:86], data_PCare_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_PCare_Urban = ts(as.numeric(data$Personal.care.and.effects[data$Sector=='Urban']), start = c(2013,1), 
                      frequency = 12)
str(data_PCare_Urban)
head(data_PCare_Urban)
summary(data_PCare_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_PCare_Urban)) # The output is 87, 88 here
data_PCare_Urban[88] = sum(data_PCare_Urban[86], data_PCare_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_PCare_Urban[87] = sum(data_PCare_Urban[85:86], data_PCare_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_PCare_Both = ts(as.numeric(data$Personal.care.and.effects[data$Sector=='Rural+Urban']), start = c(2013,1), 
                     frequency = 12)
str(data_PCare_Both)
head(data_PCare_Both)
summary(data_PCare_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_PCare_Both)) # The output is 87, 88 here
data_PCare_Both[88] = sum(data_PCare_Both[86], data_PCare_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_PCare_Both[87] = sum(data_PCare_Both[85:86], data_PCare_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(Miscellaneous)
data_Mis_Rural = ts(as.numeric(data$Miscellaneous[data$Sector=='Rural']), start = c(2013,1), 
                    frequency = 12)
str(data_Mis_Rural)
head(data_Mis_Rural)
summary(data_Mis_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_Mis_Rural)) # The output is 87, 88 here
data_Mis_Rural[88] = sum(data_Mis_Rural[86], data_Mis_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Mis_Rural[87] = sum(data_Mis_Rural[85:86], data_Mis_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_Mis_Urban = ts(as.numeric(data$Miscellaneous[data$Sector=='Urban']), start = c(2013,1), 
                    frequency = 12)
str(data_Mis_Urban)
head(data_Mis_Urban)
summary(data_Mis_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_Mis_Urban)) # The output is 87, 88 here
data_Mis_Urban[88] = sum(data_Mis_Urban[86], data_Mis_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Mis_Urban[87] = sum(data_Mis_Urban[85:86], data_Mis_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_Mis_Both = ts(as.numeric(data$Miscellaneous[data$Sector=='Rural+Urban']), start = c(2013,1), 
                   frequency = 12)
str(data_Mis_Both)
head(data_Mis_Both)
summary(data_Mis_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_Mis_Both)) # The output is 87, 88 here
data_Mis_Both[88] = sum(data_Mis_Both[86], data_Mis_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_Mis_Both[87] = sum(data_Mis_Both[85:86], data_Mis_Both[89])/3 # Manually replacing the NA value with neighborhood averages

# CPI(General)
data_General_Rural = ts(as.numeric(data$General.index[data$Sector=='Rural']), start = c(2013,1), 
                        frequency = 12)
str(data_General_Rural)
head(data_General_Rural)
summary(data_General_Rural) # Getting the summary of the data_Egg_Rural
which(is.na(data_General_Rural)) # The output is 87, 88 here
data_General_Rural[88] = sum(data_General_Rural[86], data_General_Rural[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_General_Rural[87] = sum(data_General_Rural[85:86], data_General_Rural[89])/3 # Manually replacing the NA value with neighborhood averages

data_General_Urban = ts(as.numeric(data$General.index[data$Sector=='Urban']), start = c(2013,1), 
                        frequency = 12)
str(data_General_Urban)
head(data_General_Urban)
summary(data_General_Urban) # Getting the summary of the data_Egg_Urban
which(is.na(data_General_Urban)) # The output is 87, 88 here
data_General_Urban[88] = sum(data_General_Urban[86], data_General_Urban[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_General_Urban[87] = sum(data_General_Urban[85:86], data_General_Urban[89])/3 # Manually replacing the NA value with neighborhood averages

data_General_Both = ts(as.numeric(data$General.index[data$Sector=='Rural+Urban']), start = c(2013,1), 
                       frequency = 12)
str(data_General_Both)
head(data_General_Both)
summary(data_General_Both) # Getting the summary of the data_Egg_Both
which(is.na(data_General_Both)) # The output is 87, 88 here
data_General_Both[88] = sum(data_General_Both[86], data_General_Both[89:90])/3 # Manually replacing the NA value with neighborhood averages
data_General_Both[87] = sum(data_General_Both[85:86], data_General_Both[89])/3 # Manually replacing the NA value with neighborhood averages






# setwd("C:/Users/ghosh/OneDrive/Documents")
getwd()

# Setting The Month-Year Column
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2023-03-01")
time_column <- seq.Date(start_date, end_date, by = "1 month")
print(time_column)


# For Rural Area
Rural <- cbind(data_CandP_Rural, data_MandF_Rural, data_Egg_Rural, data_Milk_Rural, 
               data_Oil_Rural, data_Fruits_Rural, data_Veg_Rural, data_Pulses_Rural, 
               data_Suger_Rural, data_Spices_Rural, data_NonAlcoho_Rural, data_Meals_Rural, 
               data_Food_Rural, data_Pan_Rural, data_Cloth_Rural, data_Foot_Rural, 
               data_CandF_Rural, data_Fuel_Rural, data_Household_Rural, data_Health_Rural, 
               data_TandC_Rural, data_RandA_Rural, data_Education_Rural, data_PCare_Rural, 
               data_Mis_Rural, data_General_Rural)

print(Rural)

# There is No Values on April 2019 (i.e. Whole Row is missing)
New_Rural_row <- (Rural[74,]+Rural[75,]+Rural[76,]+Rural[77,])/4
print(New_Rural_row)

Rural_new <- rbind(Rural[1:75,], New_Rural_row, Rural[76:nrow(Rural),])
print(Rural_new)
nrow(Rural_new)

# Adding The Time Column
Rural_CPI <- cbind(data.frame(Date = format(time_column, "%Y-%m")), data.frame(Rural_new))

# Saving as CSV File
write.csv(Rural_CPI, file = "Rural_CPI_March_2023.csv", row.names = FALSE)



# For Urban Area
Urban <- cbind(data_CandP_Urban, data_MandF_Urban, data_Egg_Urban, data_Milk_Urban, 
               data_Oil_Urban, data_Fruits_Urban, data_Veg_Urban, data_Pulses_Urban, 
               data_Suger_Urban, data_Spices_Urban, data_NonAlcoho_Urban, data_Meals_Urban, 
               data_Food_Urban, data_Pan_Urban, data_Cloth_Urban, data_Foot_Urban, 
               data_CandF_Urban, data_House_Urban, data_Fuel_Urban, data_Household_Urban, 
               data_Health_Urban, data_TandC_Urban, data_RandA_Urban, data_Education_Urban, 
               data_PCare_Urban, data_Mis_Urban, data_General_Urban)

print(Urban)

# There is No Values on April 2019 (i.e. Whole Row is missing)
New_Urban_row <- (Urban[74,]+Urban[75,]+Urban[76,]+Urban[77,])/4
print(New_Urban_row)

Urban_new <- rbind(Urban[1:75,], New_Urban_row, Urban[76:nrow(Urban),])
print(Urban_new)
nrow(Urban_new)

# Adding The Time Column
Urban_CPI <- cbind(data.frame(Date = format(time_column, "%Y-%m")), data.frame(Urban_new))

# Saving as CSV File
write.csv(Urban_CPI, file = "Urban_CPI_March_2023.csv", row.names = FALSE)



# For Rural+Urban Area
Both <- cbind(data_CandP_Both, data_MandF_Both, data_Egg_Both, data_Milk_Both, 
              data_Oil_Both, data_Fruits_Both, data_Veg_Both, data_Pulses_Both, 
              data_Suger_Both, data_Spices_Both, data_NonAlcoho_Both, data_Meals_Both, 
              data_Food_Both, data_Pan_Both, data_Cloth_Both, data_Foot_Both, 
              data_CandF_Both, data_House_Both, data_Fuel_Both, data_Household_Both, 
              data_Health_Both, data_TandC_Both, data_RandA_Both, data_Education_Both, 
              data_PCare_Both, data_Mis_Both, data_General_Both)

print(Both)

# There is No Values on April 2019 (i.e. Whole Row is missing)
New_Both_row <- (Both[74,]+Both[75,]+Both[76,]+Both[77,])/4
print(New_Both_row)

Both_new <- rbind(Both[1:75,], New_Rural_row, Both[76:nrow(Both),])
print(Both_new)
nrow(Both_new)

# Adding The Time Column
Both_CPI <- cbind(data.frame(Date = format(time_column, "%Y-%m")), data.frame(Both_new))

# Saving as CSV File
write.csv(Both_CPI, file = "Both_CPI_March_2023.csv", row.names = FALSE)

