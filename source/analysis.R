library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

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
# Getting data from csv file and getting 3 critical values.
#----------------------------------------------------------------------------#
jail_data <- read.csv("../data/incarceration_trends.csv")
View(jail_data)

# In the following calculation, "incarceration = prison"
# Value 1: What percentage of incarcerated people were black in 2016? (for the entire US)
black_percentage_2016 <- jail_data %>%
  select(year, total_prison_pop, black_prison_pop) %>% 
  filter(year == "2016") %>% 
  replace(is.na(.), 0) %>% 
  summarize(total_prison_pop = sum(total_prison_pop), black_prison_pop = sum(black_prison_pop)) %>% 
  mutate(percentage = 100 * black_prison_pop / total_prison_pop) %>% 
  pull(percentage)

# Round percentage to reasonable amount of digits
black_percentage_2016 <- round(black_percentage_2016, 2)

# Value 2: What was the average number of prisoners per county in 2016?
# Step 1: get number of counties in 2016 (That have data!!!)
num_counties <- jail_data %>% 
  select(year, total_prison_pop) %>% 
  filter(year == "2016") %>% 
  drop_na() %>% 
  nrow()

# Step 2: get total inmates in 2016
total_prisoners_2016 <- jail_data %>% 
  select(year, total_prison_pop) %>% 
  filter(year == "2016") %>% 
  drop_na() %>% 
  summarize(total_prison_pop = sum(total_prison_pop)) %>% 
  pull(total_prison_pop)

# Step 3: calculate average per state and round to reasonable digits
avg_prisoners_2016 <- round(total_prisoners_2016 / num_counties, 2)

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
# get_year_jail_pop function and plot
#----------------------------------------------------------------------------#
# This function returns total US jail population for a given vector of years
get_year_jail_pop <- function() {
  jail_population_df <- jail_data %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarize(total_jail_pop = sum(total_jail_pop))
  return(jail_population_df)   
}

# This function creates the bar chart with a given jail rate data frame. Labels 
# and titles are specific to the data
plot_jail_pop_for_us <- function()  {
  values <- get_year_jail_pop()$total_jail_pop
  
  return(ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
           geom_col() +
           labs(
             x = "Year",
             y = "Total Jail Population"
           ) +
           scale_y_continuous(labels = comma) +
           ggtitle("Increase of Jail Population in U.S. (1970-2018)"))   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# This function returns a data frame for the given state tracking change in 
# incarceration over time
get_jail_pop_by_states <- function(states) {
  state_jail_pop <- jail_data %>% 
    select(year, state, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(state, year) %>% 
    summarize(total_jail_pop = sum(total_jail_pop)) %>% 
    subset(state %in% states)
}

# This function returns a line plot for the given states. Uses get_jail_pop_by_state
# to generate a dataframe
plot_jail_pop_by_states <- function(states) {
  return(ggplot(get_jail_pop_by_states(states), aes(x=year, y=total_jail_pop, group=state)) +
                geom_line(aes(color=state)) +
                geom_point(aes(color=state))+
                labs(
                  x = "Year",
                  y = "Total Jail Population"
                ) +
                ggtitle("Jail Population Increase per State (1970-2018)"))
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# I have chosen to analyse the percent of black people per county compared to percent 
# of black people incarcerated in the same area. A balanced model would look like y = x
#----------------------------------------------------------------------------#
get_black_jail_pop_perc <- function() {
  black_incarceration_data <- jail_data %>% 
    select(state, year, black_pop_15to64, black_jail_pop, total_jail_pop, total_pop) %>% 
    filter(year == "2018") %>% 
    drop_na() %>% 
    mutate(perc_black_pop = 100 * black_pop_15to64 / total_pop, 
           perc_jail_black_pop = 100 * black_jail_pop / total_jail_pop) %>% 
    select(perc_black_pop, perc_jail_black_pop) %>% 
    filter(perc_jail_black_pop > 0 & perc_jail_black_pop < 100) 
  # There are a small number of data points that are not within the bounds of a percentage.
  # These data points are most likely errors made by the creators bc so few of them exist so
  # they will be removed
  return(black_incarceration_data)
}

# This function plots teh data frame from the previous function in a scatterplot that contains
# the data's linear distribution and the "unbiased" trend line y = x
plot_black_jail_pop_perc <- function() {
  return(ggplot(get_black_jail_pop_perc(), aes(x=perc_black_pop, y=perc_jail_black_pop)) + 
                geom_point()+
                xlim(0, 100) +
                ylim(0,100) +
                labs(
                  x = "Percentage Black Population (%)",
                  y = "Percentage Black Jail Population (%)"
                ) +
                ggtitle("Percentage Black Population and Black Jail Population in Counties (2018)") +
                geom_smooth(method=lm, se=FALSE) +
                geom_abline(intercept=0, slope=1, color="Red", linetype="dashed"))
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# This section has four functions to create two maps of Texas's county incarceration 
# percentages and totals
#----------------------------------------------------------------------------#
library(maps)
# MAP 1: Map of Texas will show percentage of total citizens that are incarcerated by county
# This function generates a map of each Texas countue's incarceration percentage
get_texas_percentage <- function() {
  texas_incarceration <- jail_data %>% 
    filter(year == "2018", state == "TX") %>% 
    select(county_name, total_jail_pop, total_pop) %>% 
    mutate(county_name = tolower(county_name)) %>% 
    mutate(percentage = 100 * total_jail_pop / total_pop) %>% 
    select(county_name, percentage)
  
  colnames(texas_incarceration) <- c("subregion", "percentage")
  
  texas_shape <- map_data("county", "texas") %>% 
    mutate(subregion = paste(subregion, "county")) %>% 
    left_join(texas_incarceration, by = "subregion")
  
  return(texas_shape)
}

# This function plots the percentage map from above
plot_texas_percentage <- function() {
  ggplot(get_texas_percentage()) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = percentage),
      color = "white",
      size = 0.1
    ) +
    coord_map() +
    scale_fill_continuous(low = "black", high = "red") +
    labs(fill = "Percentage Jail Population") +
    ggtitle("Jail Rate of Texas Counties (2018)")
}

# MAP 2: Map of Texas will show number of citizens that are incarcerated in each county
# This function generates the right data frame for the texas totals map
get_texas_jail_pops <- function() {
  texas_incarceration <- jail_data %>% 
    filter(year == "2018", state == "TX") %>% 
    select(county_name, total_jail_pop) %>% 
    mutate(county_name = tolower(county_name))
  
  colnames(texas_incarceration) <- c("subregion", "total_jail_pop")
  
  texas_shape <- map_data("county", "texas") %>% 
    mutate(subregion = paste(subregion, "county")) %>% 
    left_join(texas_incarceration, by = "subregion")
  
  return(texas_shape)
}

# This function plots texas jail totals map
plot_texas_jail_pops <- function() {
  ggplot(get_texas_jail_pops()) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
      color = "white",
      size = 0.1
    ) +
    coord_map() +
    scale_fill_continuous(low = "black", high = "red") +
    labs(fill = "Total Jail Population") +
    ggtitle("Jail Populations in Texas Counties (2018)")
}

## Load data frame ---- 


