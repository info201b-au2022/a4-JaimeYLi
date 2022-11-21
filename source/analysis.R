library(tidyverse)
library(dplyr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Getting data from github and getting 3 critical values.
#----------------------------------------------------------------------------#
jail_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(jail_data)

## In the following calculations, "incarceration = prison"
# Value 1: What percentage of incarcerated people were black in 2018?
black_percentage_2018 <- jail_data %>%
  select(year, state, total_prison_pop, black_prison_pop) %>% 
  filter(year == "2018") %>% 
  group_by(state)
  
View(black_percentage_2018)
# Value 2: What percentage of incarcerated individuals were incarcerated in the year 2018?

# Value 3: In which year did total incarceration numbers (for the entire US) increase the most?
most_increase_year <- jail_data %>% 
  select(year, total_prison_pop) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarize(total_prison_pop = sum(total_prison_pop)) %>% 
  mutate(difference = total_prison_pop - lag(total_prison_pop)) %>% 
  drop_na() %>% 
  filter(difference == max(difference)) %>% 
  pull(year)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


