# Assignment 3

trend <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggmap)

# Summary------------------------------------------------------------------------------------
# Which year has the highest jail population count in the United State?
high_year <- trend %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
high_year 
# 1993

#What is the average value of *Black people* in the jail across all the counties in 1993?
mean_black_1993 <- trend %>% 
  filter(year == "1993") %>% 
  summarize( mean = mean(black_jail_pop, na.rm = TRUE)) %>% 
  pull(mean)
mean_black_1993
# 74.1503

#What is the average value of *White people* in the jail across all the counties in 1993?
mean_white_1993 <- trend %>% 
  filter(year == "1993") %>% 
  summarize( mean = mean(white_jail_pop, na.rm = TRUE)) %>% 
  pull(mean)
mean_white_1993
# 65.67035

#What is the highest numbers in *Black people who are in jail*?
max_black_jail <- trend %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(black_jail_pop)
max_black_jail  
# 13143.92
  
#What is the highest numbers in *White people who are in jail*?  
max_white_jail <- trend %>% 
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(white_jail_pop)
max_white_jail
# 7036.59

#Which *county* has the highest number of *Black people who are in jail*?
county_high_black <- trend %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name)
county_high_black
# "Los Angeles County"
  
# Which *county* has the highest number of *White people who are in jail*?
county_high_white <- trend %>% 
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name)
county_high_white
# "Los Angeles County"

# What is the ratio of Black people who are in jail to White people who are in jail in Los Angeles County in 2018?
ratio_black_white <- trend %>% 
  filter(year == "2018" & county_name == "Los Angeles County") %>% 
  summarize(ratio = black_jail_pop / white_jail_pop) %>% 
  pull(ratio)
ratio_black_white
# 1.948042


# Trend Chart---------------------------------------------------------------------------------------
trend_one <- data.frame(
  trend %>% 
    select(year, county_name, total_jail_pop, black_jail_pop, white_jail_pop)
)

top_2_county <- trend_one %>% 
  group_by(county_name) %>% 
  summarize(max_total_pop = max(total_jail_pop)) %>% 
  arrange(-max_total_pop) %>% 
  slice(1:2) %>% 
  pull(county_name)
top_2_county
# "Los Angeles County" "New York County"


trend_two <- trend_one %>% 
  filter(county_name %in% c("Los Angeles County","New York County"))


trend_chart <- ggplot(trend_two, aes(x=year, y=black_jail_pop)) + 
  geom_line(aes(col = county_name)) +
  labs(title = "A Trend in the Top Two Counties That Has the Highest Jail Population", 
      x = "Year", y = "The Population Count of Black People in Jail", colour = "County's Name") 

# Variable Comparison Chart----------------------------------------------------------------------------
trend_three <- trend_one %>% 
  filter(county_name == "Los Angeles County")
trend_three


comapre_chart <- ggplot(trend_three, aes(x = white_jail_pop, y = black_jail_pop)) +
  geom_point(aes(col = county_name)) +
  geom_smooth(formula = y ~ x,method = lm) +
  labs(title = "The Comparison Between The Number of Black People And White People",
       x = "The Population Count of White People", y="The Population Count of Black People", 
       colour = "County With Highest Jail Population")

#Map--------------------------------------------------------------------------------------------------------
trend_four <- data.frame(
  trend_one %>% 
    filter(year == "2013") %>% 
    mutate(county_name = tolower(county_name)) %>% 
    mutate(county_name = word(county_name, 1, -2)) 
)
colnames(trend_four)[which(names(trend_four) == "county_name")] <- "subregion"

county_shape <- map_data("county")
View(county_shape)

county_shape <- left_join(county_shape, trend_four, by = "subregion")

county_shape1<-county_shape%>% 
  filter(!is.na(county_shape$black_jail_pop))
View(county_shape1)

Amercia_map <- ggplot(county_shape1, aes(x = long, y= lat, group = group)) +
  geom_polygon(aes(
    fill= black_jail_pop), 
    color = "white") +
  scale_fill_gradient(name = "% in jail", low = "yellow", high = "red") +
  labs(title = "Population of Black People In Jail") +
  theme(
    axis.line = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  ) +
  coord_map()

Amercia_map



 