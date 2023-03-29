library(tidyverse)

gapminder_data <- read.csv("data/gapminder_data.csv")

#summarize():  our data frame: summarize(data_set, new dataframe = tool(column name))

summarize(gapminder_data, averageLifeExp=mean(lifeExp))

# %>% to shorthand, type shift command m 

gapminder_data_summarized <- gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))

#max(): use max to compute summary statistics 
gapminder_data %>% summarize(recent_year=max(year))

#filter(): to subset dataframe
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(averageLifeExp=mean(lifeExp))

#EXAMPLE: min(), mean(): what is the average GDP per capita for the first year in the dataset
#1. Find what is the first year? min()
gapminder_data %>% 
  summarize(first_gdp=mean(min(year))) 

#2. Find the Average GDP per cap for that year 
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize(average_gdp=mean(gdpPercap))

#group_by(): 
gapminder_data %>% 
  group_by(year) %>% 
  summarize(average_lifeExp=mean(lifeExp))

#EXAMPLE: calculate the average life expectancy by continent 
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average_lifeExp=mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average_lifeExp=mean(lifeExp), min_lifeexp=min(lifeExp))

#mutate(): to change variable or column in a dataframe 
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

#making a new column called popInMillions that is the population in millions
gapminder_data_with_mutation <- gapminder_data %>% 
  mutate(popInMillions= pop * 0.000001)

#select(): subsets columns in a dataframe 
gapminder_data %>% 
  select(pop, year)

#to deselect, add a - 
gapminder_data %>% 
  select(-continent)

#EXAMPLE: select() create a dataframe with only country, continent, year and lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)

#oivot_wider(): takes long data and makes it wide, 
#pivot_longer(): takes wide data and makes it long 
gapminder_data %>% 
  select(-pop, -gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)


#EXAMPLE:subsetting data by value and selecting column rows of interest: the gapminder_data to the year 2007 and drop the year and continent columns
gapminder_data %>% 
  filter(year == 2007) %>% 
  select(-year, -continent)

#EXAMPLE: filtering 2 columns by value and selecting columns of interest 
gapminder_data %>% 
  filter(year == 2007) %>% 
  filter(continent == "Americas") %>% 
  select(-year, -continent)

#EXAMPLE: filtering 2 columns by row values 
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% # can use a , or & in filter
  select(-year, -continent)


#are CO2 emissions related to GDP 
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2,
         col_names= c("region", "country", "year", "series",
                      "value", "footnotes", "source"))

#mutate (series = recode("dirty value" = fixed)
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"= "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"= "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#inner_join(): combining two datasets
inner_join(gapminder_data_2007, co2_emissions)

#anti_join(): what elements are dropped from innerjoin 
anti_join(gapminder_data_2007, co2_emissions)


co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"= "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year, -Series) %>% 
  mutate(country = recode(country, "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States", 
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

anti_join(gapminder_data_2007,co2_emissions)

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% # can use a , or & in filter
  select(-year, -continent) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp * pop)/ sum(pop), 
            gdpPercap = sum(gdpPercap *pop)/sum(pop), 
            pop = sum(pop))

anti_join(gapminder_data_2007, co2_emissions)

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions)

?inner_join

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "GDP (per capita)",
       y = "CO2 emitted (per capita)")









