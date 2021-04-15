library(gapminder)
libary(dplyr)

gapminder %>% summarize(meanLifeExp = mean(lifeExp))

# Summarize to find the median life expectancy
gapminder %>% summarize(meanLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>% group_by(year)  %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% filter(year == 1957) %>% group_by(continent) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent/year combination
gapminder %>% group_by(continent, year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))
