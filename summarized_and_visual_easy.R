library(gapminder)
library(dplyr)
library(ggplot2)

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp))+ expand_limits(y = 0) + geom_point()


#### Visualizing median GDP per capita per continent over time

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent = gapminder %>% group_by(continent, year) %>% summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent))+ expand_limits(y = 0) + geom_point()

#### Comparing median life expectancy and median GDP per continent in 2007

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 = gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(medianGdpPercap = median(gdpPercap), medianLifeExp = median(lifeExp))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent))+ geom_point()
