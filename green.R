## Project:  STA 215, Spring 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: criminal minds episodes 
# Date:      2024_3_4
# Who:       Jasmine E. Green



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################

## generate summary of air dates per criminal minds episodes
mean(data$`Air date `)
sd(data$`Air date `)
hist(data$`Air date `)
summary(data$`Air date `)
mean(data$`Air date `)
sd(data$`Air date `)
min(data$`Air date `)
max(data$`Air date `)

## generate summary of gruesomeness per criminal minds episodes 
mean(data$`Gruesomeness `)
sd(data$`Gruesomeness `)
min(data$`Gruesomeness `)
max(data$`Gruesomeness `)

## generate summary for writers of criminal minds episodes 
table(data$`Written by `)
hist(data$`Written by `)

## generate summary for motive of criminal minds episodes 
table(data$`Motive `)
##################################################################################
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
ggplot(raw_data, aes(x = data$`Motive `, y = data$`Air date `)) +
  geom_boxplot() +
  labs(title = "Box Plot of Motive by Air date")
    x = "Motvie"
    y = "Air date"
    theme_minimal()
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(raw_data$`Air date`, raw_data$Gruesomeness) 
print(linear_plot)

# add x line and y line for means 
meanx <- mean(raw_data$Gruesomeness)
meany <- mean(raw_data$`Air date`)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
plot(raw_data$Gruesomeness,
     residuals(raw_data$linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h=0, col="red")
##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$`Written by `,data$`Motive `)
chisq.test(data$`Written by `,data$`Motive `)