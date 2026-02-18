# Retail Analytics Explorer
An interactive Shiny app exploring what drives customer spending 
behaviour in retail mall environments.

## Overview
This project analyses a retail customer dataset of 200 observations 
to identify relationships between customer characteristics 
(age, income, time spent in mall, etc.) and total spending.

Built initially as part of ETC1000 at Monash University, then 
extended independently to explore regression modelling and 
interactive data visualisation.

## Key Findings
- Age is a strong positive predictor of total spending (R-squared = 0.634)
- Household Income has the greatest correlation with total spending (0.88)
- The age range that spent the greatest was 37-41 years old, spending on
  average $110.14
  
## Features
- Summary statistics and distribution plots for Total Spending
- Correlation heatmaps across all numeric variables
- Interactive simple linear regression with selectable variables
- Predicted vs actual spending by age group

## Tech Stack
R, Shiny, ggplot2, sjPlot, corrplot, bslib

## Data
Retail customer survey data (200 observations) collected as part 
of Monash University coursework. Variables include age, household 
income, distance from mall, time spent, items purchased, and more.

## How to Run Locally
1. Clone this repo
2. Place the dataset in the `data/` folder
3. Open `ETC1000_ShinyApp.R` in RStudio and click Run App
