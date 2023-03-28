library(tidyverse)

#loading csv data 
gapminder_1997 <- read_csv("gapminder_1997.csv")

#summary stats on data table 
str(gapminder_1997)

#use ? before a function to open help viewer 
?read_csv

#rounding, positional arguements, both are the same:  
round(3.1415, 3)
round( x = 3.1415, digits = 2 ) 

#reading in excell files  readxl::
 
#ggplot of gapminder data
ggplot(data = gapminder_1997) + #loading data
  aes (x = gdpPercap) + #setting x axis 
  labs(x = "GDP Per Capita") + #x axis title 
  aes(y = lifeExp) + #setting y axis 
  labs(y = "Life Expectancy") + #y axis title 
  geom_point() + #type of plot 
  labs(title = "Do people in wealthy countries live longer?") + #plot title 
  aes(color = continent) + #coloring by variable 
  scale_color_brewer(palette = "Set1") +  #customizing color from Color Brewer
  aes(size = pop/1000000) + #sizing by variable 
  labs(size = "Population (in millions)") #size variable for legend 

#same plot, but now condensed so all aruguemtns called for same package 
ggplot(data = gapminder_1997) + aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)")
  



